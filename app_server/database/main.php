<?php
    ini_set('date.timezone', 'Asia/Shanghai');
    
    if ($argc < 3)
    {
        $url = $_SERVER['PHP_SELF'];  
        $filename = substr($url, strrpos($url, '/'));  
        echo "argument error, usage: $filename [update|backup|restore|dump] localhost";
    }
    
    require_once 'conf.php';
    
    $mode = $argv[1];
    $conf = $argv[2];
    
    $db_host = $db_argv[$conf]['host'];
    $db_user = $db_argv[$conf]['user'];
    $db_pass = $db_argv[$conf]['pass'];
    $db_name = $db_argv[$conf]['name'];
    $db_port = $db_argv[$conf]['port'];
    
    $db_version = init_db();
    $db = new mysqli($db_host, $db_user, $db_pass, $db_name, $db_port);
    $db->query("SET NAMES utf8;");
    
    if ($mode == "update")
    {
        update_db();
    }
    else if ($mode == "backup")
    {
        export_db(true);
    }
    else if ($mode == "restore")
    {
        require_once("backup/" . $argv[3] . ".php");
    }
    else if ($mode == "export")
    {
        update_db();
        export_db(false);
    }
    else if ($mode == "run")
    {
        require_once("update/" . $argv[3] . ".php");
    }
    else if ($mode == "clean")
    {
        exit();
    }
    
    $db->close();
    
    function init_db ()
    {
        global $db_host, $db_user, $db_pass, $db_name, $db_port;
        $db = new mysqli($db_host, $db_user, $db_pass, 'information_schema', $db_port);
        
        if ($db->connect_error)
        {
            die("open information_schema failed, [" . $db->connect_errno . "]: " . $db->connect_error);
        }
        
        $sql = "SELECT `SCHEMA_NAME` FROM `SCHEMATA` WHERE `SCHEMA_NAME` = '" . $db_name . "'";
        $result = $db->query($sql);
        $need_init_db = ($result->fetch_array(MYSQLI_ASSOC) == false);
        $result->close();
        
        $sql = "SELECT `TABLE_NAME` FROM `TABLES` WHERE `TABLE_NAME` = 'db_version' AND `TABLE_SCHEMA` = '" . $db_name . "'";
        $result = $db->query($sql);
        $need_init_tb = ($result->fetch_array(MYSQLI_ASSOC) == false);
        $result->close();
        
        if ($need_init_db)
        {
            $sql = "CREATE DATABASE `" . $db_name . "` CHARACTER SET 'utf8' COLLATE 'utf8_general_ci';\n";
            
            if ($db->query($sql) === FALSE)
            {
                die("create database failed, [" . $db->errno . "]: " . $db->error);
            }
        }
        
        $sql = "USE `" . $db_name . "`";
        $db->query($sql);
        
        if ($need_init_tb)
        {
            $sql = "CREATE TABLE `db_version` (`version` INT, `change_time` INT NOT NULL DEFAULT 0);\nINSERT INTO `db_version` VALUES (0, 0);\n";
            
            if ($db->multi_query($sql) === FALSE)
            {
                die("create database failed, [" . $db->errno . "]: " . $db->error);
            }
        }
        
        $db->close();
        $db = new mysqli($db_host, $db_user, $db_pass, $db_name, $db_port);
        
        $sql = "SELECT `version` FROM `db_version`";
        $result = $db->query($sql);
        $row = $result->fetch_array(MYSQLI_ASSOC);
        $result->close();
        
        if ($row == FALSE)
        {
            $db->query("INSERT INTO `db_version` VALUES (0, 0);");
            $version = 0;
        }
        else
        {
            $version = $row['version'];
        }
        
        $db->close();
        
        return $version;
    }
    
    function update_db () 
    {
        global $db, $db_version, $mode;
        $changes = get_changes();
        $stime = microtime(true);
        $last_dump_ver = 0;
        
        foreach (array_keys($changes) as $ver)
        {
            if ($changes[$ver]['is_dump'] && $last_dump_ver <= $ver)
            {
                $last_dump_ver = $ver;
            }
        }
        
        foreach (array_keys($changes) as $ver)
        {
            if ($db_version == 0)
            {
                if ($ver < $last_dump_ver)
                {
                    continue;
                }
            }
            else if ($ver <= $db_version || ($changes[$ver]['is_dump'] && $ver < $last_dump_ver))
            {
                continue;
            }
            
            echo "\napply change: " . $changes[$ver]['name'];
            require_once("./" . $changes[$ver]['dir'] . "/" . $changes[$ver]['name']);
            $sql = "UPDATE `db_version` SET `version` = " . $ver . ", `change_time` = UNIX_TIMESTAMP()";
            
            if ($db->query($sql) === FALSE)
            {
                die("update db_version failed");
            }
            
            echo " ........................ [done]";
        }
        
        $etime = microtime(true);
        echo "\n\napply changes complete in " . round(($etime - $stime), 2) . "s\n";
    }
    
    function get_changes ()
    {
        $changes = array();        
        get_changes_from_dir('changes', $changes);
        asort($changes);
        
        return $changes;
    }
    
    function get_changes_from_dir ($dir, &$changes)
    {
        if ($handle = opendir('./' . $dir))
        {
            while (FALSE !== ($file = readdir($handle)))
            {
                if ($file == "." || $file == ".." || is_dir($file))
                {
                    continue;
                }
                
                if (strrpos($file, ".php") == 13)
                {
                    if (strlen($file) != 17)
                    {
                        continue;
                    }
                    
                    $name = substr($file, 0, -4);
                    $id = (int)str_replace('-', '', $name);
                    $is_dump = false;
                }
                else if (strrpos($file, ".dump.php") == 13)
                {
                    if (strlen($file) != 22)
                    {
                        continue;
                    }
                    
                    $name = substr($file, 0, -4);
                    $id = (int)str_replace('.dump', '', str_replace('-', '', $name));
                    $is_dump = true;
                }
                else
                {
                    continue;
                }
                
                $changes[$id] = array('name' => $file, 'dir' => $dir, 'is_dump' => $is_dump);
            }
        }
    }
    
    function export_db ($is_backup = false)
    {
        global $db, $db_host, $db_user, $db_pass, $db_name, $db_port;
        $stime = microtime(true);
        
        if ($is_backup)
        {
            $output_file = str_replace('\\', '/', getcwd()) . "/backup/" . date("Y-m-d_H-i-s") . ".php";
        }
        else
        {
            $changes = get_changes();
            $i = 0;
            
            while (true)
            {
                $i += 1;
                $file_name = date("Y-m-d") . "-" . sprintf("%02d", $i);
                $ver = (int)str_replace('-', '', $file_name);
                
                if (array_key_exists($ver, $changes) == false)
                {
                    break;
                }
            }
            
            $output_file = str_replace('\\', '/', getcwd()) . "/changes/" . $file_name . ".dump.php";
        }
        
        $file = fopen($output_file, 'c');
        $tables = get_tables($db_host, $db_user, $db_pass, $db_name, $db_port);
        $max_length = get_max_length($tables);
        
        fwrite($file, "<?php\n\n");
        fwrite($file, "echo \"\\n\\n\";\n");
        
        fwrite($file, "
            execute(\"
            /*!40101 SET NAMES utf8 */;

            /*!40101 SET SQL_MODE=''*/;

            /*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
            /*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
            /*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
            /*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;
            \");
        ");
        
        foreach ($tables as $table)
        {
            if ($table == "db_version")
            {
                continue;
            }
            
            if ($is_backup)
            {
                echo "backup ";
            }
            else
            {
                echo "dump ";
            }
            
            echo $table . " ";
            $dots = generate_char($max_length, strlen($table), '.');
            echo $dots . "......... ";
            
            $sql = get_create_table_sql($db, $table);
            
            fwrite($file, 'if ($db_version == 0)' . "\n{\n");
            fwrite($file, "    echo \"    " . $table . " " . $dots . "......... \";\n\n");
            fwrite($file, "    execute(\"\n");
            fwrite($file, $sql);
            fwrite($file, "\n\");\n\n");
            fwrite($file, "    echo \"[created]\\n\"; \n");
            fwrite($file, "}\n\n");
            
            if ($is_backup == false && strpos($table, "player") === 0)
            {
                echo "[ignore]\n";
                continue;
            }
            
            $fields = get_table_fields($db, $table);
            $sql = get_insert_into_sql($db, $table, $fields);
            
            fwrite($file, "echo \"    " . $table . " " . $dots . "......... \";\n\n");
            fwrite($file, "execute(\"DELETE FROM `" . $table . "`\");\n\n");
            
            if ($sql != "")
            {
                fwrite($file, "execute(\"\n");
                fwrite($file, $sql);
                fwrite($file, "\");\n\n");
            }
            
            fwrite($file, "echo \"[loaded]\\n\"; \n");
            fwrite($file, "\n");
            
            echo "[done]\n";
        }
        
        fwrite($file, "
            execute(\"
            /*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
            /*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
            /*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
            /*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
            \");
        ");
        
        fwrite($file, "echo \"\\n\";\n");
        fwrite($file, "?>\n");
        fclose($file);
        
        if ($is_backup == false)
        {
            $sql = "UPDATE `db_version` SET `version` = " . $ver . ", `change_time` = UNIX_TIMESTAMP()";
            
            if ($db->query($sql) === FALSE)
            {
                die("can't update db_version");
            }
        }
        
        $etime = microtime(true);
        echo "\ndatabase ";
        
        if ($is_backup)
        {
            echo "backup ";
        }
        else
        {
            echo "dump ";
        }
        
        echo "complete in " . round(($etime - $stime), 2) . "s\n";
    }
    
    function get_tables($db_host, $db_user, $db_pass, $db_name, $db_port)
    {
        $info_schema = new mysqli($db_host, $db_user, $db_pass, 'information_schema', $db_port);
        
        if ($info_schema->connect_error)
        {
            die("open information_schema failed, [" . $info_schema->connect_errno . "]: " . $info_schema->connect_error);
        }
        
        $sql = "SELECT `TABLE_NAME` FROM `TABLES` WHERE `TABLE_SCHEMA` = '$db_name'";
        $result = $info_schema->query($sql);
        $tables = array();
        
        while ($row = $result->fetch_array(MYSQLI_ASSOC))
        {
            $tables[] = $row['TABLE_NAME'];
        }
        
        $result->close();
        $info_schema->close();
        
        return $tables;
    }
    
    function get_max_length ($tables)
    {
        $max_length = 0;
        
        foreach ($tables as $table)
        {
            $len = strlen($table);
            
            if ($len > $max_length)
            {
                $max_length = $len;
            }
        }
        
        return $max_length;
    }
    
    function generate_char ($max_len, $len, $char)
    {
        $str = '';
        
        for ($i = 0; $i < ($max_len - $len); $i++)
        {
            $str .= $char;
        }
        
        return $str;
    }
    
    function get_create_table_sql ($db, $table)
    {
        $sql = "SHOW CREATE TABLE `" . $table . "`";
        $result = $db->query($sql);
        $row = $result->fetch_array(MYSQLI_ASSOC);
        $create_table_sql = $row['Create Table'] . ";";
        $result->close();
        
        return $create_table_sql;
    }
    
    function get_table_fields ($db, $table)
    {
        $sql = "SHOW FIELDS FROM `" . $table . "`";
        $result = $db->query($sql);
        $fields = array();
        
        while ($row = $result->fetch_array(MYSQLI_ASSOC))
        {
            $fields[] = $row['Field'];
        }
        
        $result->close();
        
        return $fields;
    }
    
    function get_insert_into_sql ($db, $table, $fields)
    {
        $insert_into_sql = "INSERT INTO `" . $table . "` (" . implode("`,`", $fields) . "`) VALUES";
        $sql = "SELECT `" . implode("`,`", $fields). "` FROM `". $table . "`";
        $result = $db->query($sql, MYSQLI_USE_RESULT);
        $insert_rows = array();
        
        while ($row = $result->fetch_array(MYSQLI_ASSOC))
        {
            $values = array();
            
            foreach ($fields as $field)
            {
                $values[] = str_replace(array('"', '\''), array('\"', '\\\''), $row[$field]);
            }
            
            $insert_rows[] = "\n('" . implode("','", $values) . "')";
        }
        
        if (count($insert_rows) == 0)
        {
            $result->close();
            return "";
        }
        
        $insert_into_sql .= implode(',', $insert_rows) . ";\n";
        $result->close();
        
        return $insert_into_sql;
    }
    
    function query ($sql)
    {
        global $db;
        $result = $db->query($sql, MYSQLI_USE_RESULT);
        
        if (!$result)
        {
            die("Query Error, [" . $db->errno . "]: " . $db->error);
        }
        
        $row = array();
        
        while ($row = $result->fetch_array(MYSQLI_ASSOC))
        {
            $rows[] = $row;
        }
        
        $result->close();
        
        return $rows;
    }
    
    function query_array ($sql)
    {
        global $db;
        $result = $db->query($sql, MYSQLI_USE_RESULT);
        
        if (!$result)
        {
            die("Query Error, [" . $db->errno . "]: " . $db->error);
        }
        
        $row = array();
        
        while ($row = $result->fetch_array(MYSQLI_NUM))
        {
            $rows[] = $row;
        }
        
        $result->close();
        
        return $rows;
    }
    
    function free_result ()
    {
        global $db;
        
        do
        {
            if ($result = $db->store_result())
            {
                $result->free();
            }
            
            if (!$db->more_results())
            {
                break;
            }
        }
        while ($db->next_result());
    }
    
    function execute ($sql)
    {
        global $db;
        
        if (!$db->multi_query($sql))
        {
            echo "\nsql execute failure\n-----------------\n$sql\n------------\n";
            printf("Error - SQLSTATE %d: %s.\n", $db->errno, $db->error);
            echo ("!!!!!!!!!!!!!!!!!!!\n");
            die('error occur, exited.');
        }
        
        free_result();
    }
    
    function last_insert_id ()
    {
        global $db;
        
        return $db->insert_id;
    }
?>