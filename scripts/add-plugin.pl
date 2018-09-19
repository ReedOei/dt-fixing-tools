#!/usr/bin/env swipl

:- use_module(library(achelois)).

:- initialization(main, main).

dtfixingtools_plugin(
    plugin('com.reedoei':'testrunner-maven-plugin':'0.1-SNAPSHOT',
           [className='edu.illinois.cs.dt.tools.diagnosis.Diagnoser'],
           [dependency('edu.illinois.cs':'dt-fixing-tools':'1.0.0-SNAPSHOT', [], [], _)],
           _)).

main(_Argv) :-
    working_directory(CWD, CWD),

    pom('pom.xml', Pom),

    (
        not((
            artifacts(Pom, plugins, Plugin),
            maven_xml:coords(Plugin, 'com.reedoei':'testrunner-maven-plugin':_)
            )) ->

            dtfixingtools_plugin(PluginToAdd),
            add_artifact(plugins, PluginToAdd, Pom, NewPom),
            pom('pom.xml', NewPom); % Write it out again

        true
    ).

