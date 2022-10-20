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
	Common.RiskPoints
	{
		// suffix: ""
		Layout.columnSpan: 2
	}

	Common.ProbDefect
	{
		// suffix: ""
		Layout.columnSpan: 2
	}

	RadioButtonGroup
	{
		title: qsTr("Distribution")
		name: "distribution"
		RadioButton { value: "binom"; label: qsTr("Binomial"); checked: true }
		Group
		{
			RadioButton { value: "hypergeom"; label: qsTr("Hypergeometric"); id: hypergeom}
			IntegerField { name: "lotSize"; label: qsTr("Lot size (N): "); defaultValue: 1000; min: 1; enabled: hypergeom.checked }
		}
		RadioButton { value: "poisson"; label: qsTr("Poisson") }
	}
	
	Group
	{
		Layout.rowSpan: 2
		CheckBox { name: "showOCCurve"; label: qsTr("Show OC Curve") }
		CheckBox { name: "showSummary"; label: qsTr("Show summary") }
	}	
}