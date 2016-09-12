<?php

require __DIR__ . '/../vendor/autoload.php';

session_start();

// Instantiate the app
$settings = require __DIR__ . '/../src/settings.php';
$app = new \Slim\App($settings);

// Set up dependencies
require __DIR__ . '/../src/dependencies.php';

// Register routes
require __DIR__ . '/../src/routes.php';
/*
spl_autoload_register(function ($classname) {
   require(__DIR__ . "/../models/" . $classname . ".php");
});
*/

require(__DIR__ . "/../models/AnalysisPostData.php");
require(__DIR__ . "/../models/BasicForensicsAnalysis.php");
require(__DIR__ . "/../models/ClusterMaps.php");
require(__DIR__ . "/../models/ClusterMapsPostData.php");
require(__DIR__ . "/../models/FileManager.php");

$app->run();



