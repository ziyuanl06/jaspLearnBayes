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
import JASP
import JASP.Controls
import QtQuick
import QtQuick.Layouts

Form {
    id: form
    columns: 2      ///Question 0331: Why this is not working?
    
    LS.LSintrotext{}
    ColorPalette{}

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

        RowLayout{
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


        

        Group {
            visible: inputType.value === "randsamp"
            title: qsTr("Random Sample")

            RowLayout {
                spacing: 60

                FormulaField {
                    id: rdsampleinput
                    name: "nsample"
                    label: qsTr("Sample Size")
                    min: 1
                    defaultValue: 1
                    fieldWidth: 45
                }

                Button {text: qsTr("Generate sample")  ; onClicked: {  redrawTrigger.value = redrawTrigger.value + 1; }}
                Button {text: qsTr("Reset")             ; onClicked: { redrawTrigger.value = 0; rdsampleinput.value = 1;  resetSample.value = resetSample.value + 1}}

            }

            IntegerField {
                id: redrawTrigger

                name: "redrawTrigger"
                defaultValue: 0
                visible: false
            }

            IntegerField{
                id: resetSample
                name: "resetSample"
                defaultValue: 0
                visible: false
            }

        }

    }

}
