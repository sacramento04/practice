<?php
    execute("
        DROP TABLE IF EXISTS `player`;
        
        CREATE TABLE `player`
        (
            `id`        INTEGER     AUTO_INCREMENT      COMMENT 'id',
            `username`  VARCHAR(32) NOT NULL DEFAULT '' COMMENT '用户名',
            `password`  VARCHAR(32) NOT NULL DEFAULT '' COMMENT '密码',
        
            PRIMARY KEY (`id`)
        )
        ENGINE=InnoDB AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT='玩家表';
    ");
?>