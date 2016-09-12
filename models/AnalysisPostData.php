<?php 

class AnalysisPostData
{
    public $candidates;
    public $level;
    public $registered;
    public $votes;
    public $methods;
    public $userSelectedFile;
    /**
     * Accept an array of data matching properties of this class
     * and create the class
     *
     * @param array $data The data to use to create
     */
    public function __construct(array $data) {
        $this->candidates = $data['candidates'];
        $this->level = $data['level'];
        $this->registered = $data['registered'];
        $this->votes = $data['votes'];
        $this->methods = $data['methods'];
        $this->userSelectedFile = $data['userSelectedFile'];
    }
    public function getCandidates() {
        return $this->candidates;
    }
    public function getLevel() {
        return $this->level;
    }
    public function getRegistered() {
        return $this->registered;
    }
    public function getVotes() {
        return $this->votes;
    }
    public function getMethods() {
        return $this->methods;
    }
    public function getUserSelectedFile() {
        return $this->userSelectedFile;
    }

}