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
		AvailableVariablesList	{ name: "allVariablesList"								}
		AssignedVariablesList	{ name: "variables";		title: qsTr("Variables")	}
		// AssignedVariablesList	{ name: "splitBy";			title: qsTr("Split");		singleVariable: true; suggestedColumns: ["ordinal", "nominal"];	id: splitBy }
	}

	Group
	{
		Layout.columnSpan: 2
		// IntegerField { name: "sampleSize"; label: qsTr("Sample size (n): "); defaultValue: 1; min: 1 }
		DoubleField { name: "kValue"; label: qsTr("k value"); defaultValue: 0 }
	}

	Group
	{
		columns: 2
		Layout.columnSpan: 2
		CheckBox { name: "lsl"; label: qsTr("Lower Specification Limit (LSL): "); id: lsl; checked: false }
		DoubleField{ name: "lower_spec"; label: qsTr(""); defaultValue: 0; enabled: lsl.checked /*; min: 0; max: 1; negativeValues: false;*/ }
		CheckBox { name: "usl"; label: qsTr("Upper Specification Limit (USL): "); id: usl; checked: false }
		DoubleField { name: "upper_spec"; label: qsTr(""); enabled: usl.checked; /*defaultValue: null; min: 0; max: 1; negativeValues: false;*/ }
	}

	Group
	{
		Layout.columnSpan: 2
		columns: 2
		CheckBox { name: "sd"; label: qsTr("Standard Deviation (Historical) known "); id: sd; checked: true }
		DoubleField { name: "stdev"; label: qsTr(""); enabled: sd.checked; defaultValue: 1; min: 0; negativeValues: false }
	}

	Common.RiskPoints
	{
		enabled: lsl.checked && usl.checked && sd.checked
		Layout.columnSpan: 2
	}
}