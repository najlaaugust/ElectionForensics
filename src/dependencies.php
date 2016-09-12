<?php

$container = $app->getContainer();

// view renderer
$container['renderer'] = function ($c) {
    $settings = $c->get('settings')['renderer'];
    return new Slim\Views\PhpRenderer($settings['template_path']);
};

//monolog
$container['logger'] = function($c) {
    $settings = $c->get('settings')['logger'];
    $logger = new \Monolog\Logger($settings['name']);
    $file_handler = new \Monolog\Handler\StreamHandler($settings['path']);
    $logger->pushProcessor(new Monolog\Processor\UidProcessor());
    $logger->pushHandler($file_handler, Monolog\Logger::DEBUG);
    return $logger;
};

