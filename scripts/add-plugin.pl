#!/usr/bin/env swipl

:- use_module(library(achelois)).

:- initialization(main, main).

dtfixingtools_plugin(
    Clz,
    plugin('com.reedoei':'testrunner-maven-plugin':'0.1-SNAPSHOT',
           [className=ClassName],
           [dependency('edu.illinois.cs':'dt-fixing-tools':'1.0.0-SNAPSHOT', [], [], _)],
           _)) :-
    atom_concat('edu.illinois.cs.dt.tools.', Clz, ClassName).

main(Argv) :-
    working_directory(CWD, CWD),

    pom('pom.xml', Pom),

    (
        not((
            artifacts(Pom, plugins, Plugin),
            maven_xml:coords(Plugin, 'com.reedoei':'testrunner-maven-plugin':_)
            )) ->

            (
                Argv = [Clz|_] -> dtfixingtools_plugin(Clz, PluginToAdd);
                dtfixingtools_plugin('diagnosis.DiagnoserPlugin', PluginToAdd)
            ),

            add_artifact(plugins, PluginToAdd, Pom, NewPom),
            pom('pom.xml', NewPom); % Write it out again

        true
    ).

