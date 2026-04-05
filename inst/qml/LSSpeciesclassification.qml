// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.

import "../qml/qml_components" as LS
import QtQuick 2.12
import QtQuick.Layouts 1.12
import JASP.Controls 1.0
import JASP 1.0

Form {
    id: form
    columns: 2      ///Question 0331: Why this is not working?

    LS.LSintrotext {}
    ColorPalette {}

    RadioButtonGroup {
        id: inputType

        name: "inputType"
        columns: 3
        title: qsTr("Input type")

        RadioButton {
            name: "randsamp"
            label: qsTr("Random Sampling")
            checked: true
        }

        RadioButton {
            name: "speandnum"
            label: qsTr("Enter Species and Number")
        }

        RadioButton {
            name: "sampseq"
            label: qsTr("Enter Sequence")
        }

    }

    Section {
        expanded: true
        title: qsTr("Data")


        Group {
            visible: inputType.value === "randsamp"
            title: qsTr("Random Sample")


            RowLayout {
                spacing: 50
                CheckBox {
                    id: selectIslandBox
                    name: "selectisland"
                    label: qsTr("Select an Island")
                    checked: false
                }

                IntegerField {
                    id: islandidField
                    name: "islandid"
                    label: qsTr("Enter Island ID")
                    enabled: selectIslandBox.checked
                    defaultValue: 1
                    min: 1
                }

            }

            RowLayout {
                spacing: 60

                IntegerField {
                    id: rdsampleinput
                    name: "nsample"
                    label: qsTr("Sample Size")
                    min: 1
                    defaultValue: 1
                    fieldWidth: 45
                }

                Button {text: qsTr("Generate sample") ; onClicked: { redrawTrigger.value = redrawTrigger.value + 1; }}
                Button {text: qsTr("Reset") ; onClicked: { redrawTrigger.value = 0; rdsampleinput.value = 1; resetSample.value = resetSample.value + 1}}

            }

            IntegerField {
                id: redrawTrigger

                name: "redrawTrigger"
                defaultValue: 0
                visible: false
            }

            IntegerField {
                id: resetSample
                name: "resetSample"
                defaultValue: 0
                visible: false
            }


        }
        Group {
            title: qsTr("Bar Graph")

            RowLayout {
                spacing: 80

                CheckBox {
                    name: "barBatch"
                    label: qsTr("the Current Batch")
                    checked: false
                }

                CheckBox {
                    name: "barAllSample"
                    label: qsTr("Overall")
                    checked: false
                }
            }

        }

    }

    Section {
        expanded: true
        title: qsTr("Model")

        ColumnLayout {
            spacing: 0
            Layout.preferredWidth: parent.width

            RowLayout {
                Label {text: qsTr("Model");            Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 90 * preferencesModel.uiScale}
                Label {text: qsTr("Distribution");     Layout.preferredWidth: 100 * preferencesModel.uiScale}
                Label {text: qsTr("Parameter");        Layout.preferredWidth: 130 * preferencesModel.uiScale}
                Label {text: qsTr("Min and Max");      Layout.preferredWidth: 142 * preferencesModel.uiScale}
                Label {text: qsTr("Explain");          Layout.preferredWidth: 62 * preferencesModel.uiScale}
                Label {text: qsTr("Plot");             }
            }

            ComponentsList{
                name: "models"
                defaultValues: []
                rowComponent: RowLayout{
                    spacing:  0
                    Row{
                        Layout.preferredWidth:   95 * preferencesModel.uiScale
                        TextField{
                            label:               ""
                            name:                "name"
                            startValue:			 qsTr("Model ") + (rowIndex + 1)
                            fieldWidth:			 85 * preferencesModel.uiScale
                            useExternalBorder:   false
                            showBorder:          true
                            toolTip:             qsTr("How would you like to name this model?")
                        }
                    }

                    Row{
                        Layout.preferredWidth:   105 * preferencesModel.uiScale

                        DropDown{
                            id: typeItem
                            name: "type"
                            useExternalBorder: true
                            fieldWidth:			 95 * preferencesModel.uiScale
                            values:
                            [
                                { label: qsTr("Point"),                value: "point"},
                                { label: qsTr("Uniform"),              value: "uniform"},
                                { label: qsTr("Poisson"),              value: "poisson"},
                                { label: qsTr("Negative Binomial"),    value: "negbino"}
                            ]
                        }
                    }

                    Row {
                        Layout.preferredWidth: 135 * preferencesModel.uiScale
                        spacing:               4 * preferencesModel.uiScale
                        IntegerField{
                            label:             qsTr("N")
                            name:              "pointPriorN"
                            visible:           typeItem.currentValue === "point"
                            value:             1
                            min:               1
                            fieldWidth:        50 * preferencesModel.uiScale
                            inclusive:         JASP.MinOnly
                            useExternalBorder: false
                            showBorder:        true
                            toolTip:           qsTr("How many different species do you think are there on the island?")

                        }


                        IntegerField{
                            label:             qsTr("λ")
                            name:              "poissonlambda"
                            visible:           typeItem.currentValue === "poisson"
                            fieldWidth:        50 * preferencesModel.uiScale
                            value:             1
                            min:               1
                            inclusive:         JASP.MinOnly
                            useExternalBorder: false
                            showBorder:        true
                            toolTip:           qsTr("What is the most likely number of species on the island?")

                        }


                        IntegerField{
                            label:             qsTr("μ")
                            name:              "nbMu"
                            visible:           typeItem.currentValue === "negbino"
                            fieldWidth:        50 * preferencesModel.uiScale
                            value:             1
                            min:               1
                            inclusive:         JASP.MinOnly
                            useExternalBorder: false
                            showBorder:        true
                            toolTip:           qsTr("What is the most likely number of species on the island?")

                        }

                        IntegerField{
                            label:             qsTr("ϕ")
                            name:              "nbPhi"
                            visible:           typeItem.currentValue === "negbino"
                            fieldWidth:        50 * preferencesModel.uiScale
                            value:             1
                            min:               0
                            inclusive:         JASP.None
                            useExternalBorder: false
                            showBorder:        true
                            toolTip:           qsTr("How certain your are? Enter a positive value, larger values allow for more uncertainty")

                        }



                        
                    }
                    Row{
                        spacing:               4 * preferencesModel.uiScale
                        Layout.preferredWidth: 155 * preferencesModel.uiScale
                        IntegerField{
                            label:             qsTr("Min")
                            name:              "minimum"
                            fieldWidth:        40 * preferencesModel.uiScale
                            value:             1
                            min:               1
                            inclusive:         JASP.MinOnly
                            useExternalBorder: false
                            showBorder:        true
                            toolTip:           qsTr("What do you think is the minimum posible number of different species on the island?")

                        }

                        IntegerField{
                            label:             qsTr("Max")
                            name:              "maximum"
                            fieldWidth:        40 * preferencesModel.uiScale
                            value:             1
                            min:               1
                            inclusive:         JASP.MinOnly
                            useExternalBorder: false
                            showBorder:        true
                            toolTip:           qsTr("What do you think is the maximum posible number of different species on the island?")

                        }
                    }
                    Row {
                            Layout.preferredWidth: 55 * preferencesModel.uiScale
                            CheckBox {
                                name:    "showExplain"
                                label:   "" 
                                toolTip: qsTr("Check to display a text explanation of this model in the results.")
                            }
                    }
                    Row {
                            Layout.preferredWidth: 55 * preferencesModel.uiScale
                            CheckBox {
                                name:    "showPlot"
                                label:   "" 
                                toolTip: qsTr("Check to display a distribution plot of this model in the results.")
                            }
                    }
                }

            }
        }


    }

}
