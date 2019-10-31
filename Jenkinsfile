pipeline {
  agent { label 'windows' }
  options {
    buildDiscarder(logRotator(numToKeepStr:'5'))
    timeout(time: 10, unit: 'MINUTES')
    skipDefaultCheckout()
  }
  stages {
    stage ('Build') {
      steps {
        checkout([ $class: 'GitSCM', branches: scm.branches, extensions: scm.extensions + [[$class: 'CleanCheckout']], userRemoteConfigs: scm.userRemoteConfigs ])
        withEnv(["DLC=${tool name: 'OpenEdge-12.1', type: 'openedge'}"]) {
          bat "%DLC%\\ant\\bin\\ant -DDLC=%DLC% -lib %DLC%\\pct\\pct.jar init build test dist"
        }
        junit 'results.xml'
        // stash name: 'windows-build', includes: 'target/DataDigger.zip'
        archiveArtifacts artifacts: 'target/DataDigger.zip'
      }
    }

    stage ('Code analysis') {
      steps {
        script {
          withEnv(["PATH+SCAN=${tool name: 'SQScanner4', type: 'hudson.plugins.sonar.SonarRunnerInstallation'}/bin", "DLC=${tool name: 'OpenEdge-12.0', type: 'openedge'}"]) {
            withSonarQubeEnv('RSSW') {
              if (("master" == env.BRANCH_NAME) || ("develop" == env.BRANCH_NAME)) {
                bat "sonar-scanner -Dsonar.oe.dlc=%DLC% -Dsonar.branch.name=%BRANCH_NAME%"
              } else {
                bat "sonar-scanner -Dsonar.oe.dlc=%DLC% -Dsonar.branch.name=%BRANCH_NAME% -Dsonar.branch.target=develop"
              }
            }
          }
        }
      }
    }
  }
}
