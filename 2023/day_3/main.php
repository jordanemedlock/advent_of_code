<?php

function get_part_locations($grid) {
    $indexes = [];
    foreach ($grid as $y => $line) {
        foreach (str_split($line) as $x => $char) {
            if (!str_contains("1234567890.\n", $char)) {
                $indexes[] = ['x' => $x, 'y' => $y];
            }
        }
    }
    return $indexes;
}

function get_neighbor_locations($index) {
    $locs = [];
    for ($i = -1; $i <= 1; $i++) {
        for ($j = -1; $j <= 1; $j++) {
            if ($i == 0 && $j == 0) {
                continue;
            }
            $locs[] = ['x' => $index['x'] + $i, 'y' => $index['y'] + $j];
        }
    }
    return $locs;
}

// PHP is making this not fun lol
function 

$handle = fopen("input.txt", "r");
$grid = [];
while (($line = fgets($handle)) !== false) {
    $grid[] = $line;
}
fclose($handle);

$parts = get_part_locations($grid);

echo json_encode($parts);

?> 