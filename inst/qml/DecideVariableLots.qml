//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program. If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import "./common" as Common

Form
{
	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList"; id: allVariablesList }
		AssignedVariablesList	{ name: "variables"; title: qsTr("Variables"); id: variables; singleVariable: true; suggestedColumns: ["scale"]; allowedColumns: ["scale"] }
	}

	Group
	{
		Layout.columnSpan: 2
		DoubleField { name: "kValue"; label: qsTr("k value"); defaultValue: 1; min: 0; negativeValues: false }
	}

	Group
	{
		enabled: variables.count != 1
		Layout.columnSpan: 2
		CheckBox { name: "sampleStats"; label: qsTr("Provide sample statistics directly (required if dataset is not available)"); id: sampleStats; checked: false }
		IntegerField { name: "sampleSize"; label: qsTr("Sample size (n): "); defaultValue: 1; min: 1 }
		DoubleField { name: "sampleMean"; label: qsTr("Sample mean"); defaultValue: 0 }
		DoubleField { name: "sampleSD"; label: qsTr("Sample standard deviation"); defaultValue: 1; min: 0 }
	}

	Group
	{
		columns: 2
		Layout.columnSpan: 2
		CheckBox { name: "lsl"; label: qsTr("Lower Specification Limit (LSL): "); id: lsl; checked: false }
		DoubleField{ name: "lower_spec"; label: qsTr(""); defaultValue: 0; enabled: lsl.checked; min: -Inf; negativeValues: true /*; max: 1*/ }
		CheckBox { name: "usl"; label: qsTr("Upper Specification Limit (USL): "); id: usl; checked: false }
		DoubleField { name: "upper_spec"; label: qsTr(""); enabled: usl.checked; min: -Inf; negativeValues: true /*defaultValue: null; max: 1*/ }
	}

	Group
	{
		Layout.columnSpan: 2
		columns: 2
		CheckBox { name: "sd"; label: qsTr("Standard Deviation (Historical) known "); id: sd; checked: false }
		DoubleField { name: "stdev"; label: qsTr(""); enabled: sd.checked; defaultValue: 1; min: 0; negativeValues: false }
	}

	Group
    {
		enabled: lsl.checked && usl.checked && sd.checked
		Layout.columnSpan: 2
        columns: 2
        Text { text: qsTr("Acceptable Quality Level (AQL): ") }
        DoubleField{ name: "pd_prp"; label: qsTr(""); negativeValues: false; defaultValue: 0.05; min: 0; max: 1 }
        Text { text: qsTr("Rejectable Quality Level (RQL / LTPD): ") }
        DoubleField { name: "pd_crp"; label: qsTr(""); negativeValues: false; defaultValue: 0.15; min: 0; max: 1 }
	}
}