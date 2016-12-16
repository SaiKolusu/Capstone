<?php
$my_file = 'data_file.json';
$handle = fopen($my_file, 'r');
$old_data = fread($handle,filesize($my_file));
$json = json_decode($old_data);

$coords = explode(',', $_GET['data']);
$arr = array('lat' => $coords[0], 'long' => $coords[1]);
$json->items[] = (object) $arr;

$handle = fopen("data_file.json", "w");
fwrite($handle, json_encode($json));
fclose($handle);
?>