<?php
$my_file = 'data_route_file.json';

$handle = fopen("data_route_file.json", "w");
fwrite($handle, json_encode($_GET));
fclose($handle);
?>