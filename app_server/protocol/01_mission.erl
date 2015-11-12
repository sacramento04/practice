%% 01_mission
#{
name => mission,
id => 1,

action => [
    #{
        name => get_player_mission_info,
        id => 1,
        
        in => [
            {mission_main_id, int}
        ],
        
        out => [
            {mission_info_list, list, player_mission_info}
        ]
    },
    
    #{
        name => pick_up_item,
        id => 2,
        
        in => [
            {player_mission_area_item_id, int}
        ],
        
        out => [
            {result, enum, ['SUCCESS', 'FAILED']},
            {src_item_id, int},
            {des_item_id, int},
            {item_color, byte},
            {item_value, int},
            {gift, class, star_gift},
            {gift_id, int}
        ]
    },
    
    #{
        name => test_list,
        id => 3,
        
        in => [
        ],
        
        out => [
            {result, list, [
                {a, int},
                {b, int},
                {c, string}
            ]}
        ]
    }
],

class => [
    #{
        name => player_mission_info,
        
        field => [
            {mission_id, int},
            {star, int},
            {left_sweep, int},
            {complete, byte},
            {keepers, byte},
            {mission_area, list, player_mission_area_info}
        ]
    },
    
    #{
        name => player_mission_area_info,
        
        field => [
            {mission_area_id, int},
            {mission_area_item_list, list, mission_area_item}
        ]
    },
    
    #{
        name => mission_area_item,
        
        field => [
            {player_mission_area_item_id, int},
            {mission_area_item_id, int}
        ]
    },
    
    #{
        name => star_gift,
        
        field => [
            {item_id, int},
            {item_color, byte},
            {item_amount, int}
        ]
    }
]
}.