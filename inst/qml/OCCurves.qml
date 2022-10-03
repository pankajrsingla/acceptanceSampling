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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0

Form
{
	Section
	{
		Layout.rowSpan: 2
		Layout.columnSpan: 2
		title: qsTr("First OC Curve")
		
		IntegerField { name: "lotSize1"; label: qsTr("Lot size (N): "); defaultValue: 100; min: 1; Layout.columnSpan: 2; enabled: hypergeom1.checked }
		IntegerField { name: "sampleSize1"; label: qsTr("Sample size (n): "); defaultValue: 10; min: 1; Layout.columnSpan: 2 }
		IntegerField { name: "acceptNumber1"; label: qsTr("Acceptance number (c): "); defaultValue: 4; Layout.columnSpan: 2 }
		IntegerField { name: "rejectNumber1"; label: qsTr("Rejection number (r): "); defaultValue: 5; Layout.columnSpan: 2 }

		RadioButtonGroup
		{
			title: qsTr("Distribution")
			name: "distribution1"
			RadioButton { value: "binom";		label: qsTr("Binomial"); id: binom1; checked: true }
			RadioButton { value: "hypergeom";	label: qsTr("Hypergeometric"); id: hypergeom1 }
			RadioButton { value: "poisson";		label: qsTr("Poisson"); id: poisson1 }
		}

		CheckBox{ label: qsTr("Show summary (first plan)"); name: "showSummary1"}
	}

	Section
	{
		// Layout.rowSpan: 2
		title: qsTr("Second OC Curve")

		IntegerField { name: "lotSize2"; label: qsTr("Lot size (N): "); defaultValue: 1; min: 1; Layout.columnSpan: 2; enabled: hypergeom2.checked }
		IntegerField { name: "sampleSize2"; label: qsTr("Sample size (n): "); defaultValue: 1; min: 1; Layout.columnSpan: 2 }
		IntegerField { name: "acceptNumber2"; label: qsTr("Acceptance number (c): "); defaultValue: 0; Layout.columnSpan: 2 }
		IntegerField { name: "rejectNumber2"; label: qsTr("Rejection number (r): "); defaultValue: 0; Layout.columnSpan: 2 }
	
		RadioButtonGroup
		{
			title: qsTr("Distribution")
			name: "distribution2"
			RadioButton { value: "binom";		label: qsTr("Binomial"); id: binom2; checked: false }
			RadioButton { value: "hypergeom";	label: qsTr("Hypergeometric"); id: hypergeom2 }
			RadioButton { value: "poisson";		label: qsTr("Poisson"); id: poisson2 }
		}

		CheckBox{ label: qsTr("Show summary (second plan)"); name: "showSummary2"}
	}

	Section
	{
		// Layout.rowSpan: 2
		title: qsTr("Third OC Curve")

		IntegerField { name: "lotSize3"; label: qsTr("Lot size (N): "); defaultValue: 1; min: 1; Layout.columnSpan: 2; enabled: hypergeom3.checked }
		IntegerField { name: "sampleSize3"; label: qsTr("Sample size (n): "); defaultValue: 1; min: 1; Layout.columnSpan: 2 }
		IntegerField { name: "acceptNumber3"; label: qsTr("Acceptance number (c): "); defaultValue: 0; Layout.columnSpan: 2 }
		IntegerField { name: "rejectNumber3"; label: qsTr("Rejection number (r): "); defaultValue: 0; Layout.columnSpan: 2 }

		RadioButtonGroup
		{
			title: qsTr("Distribution")
			name: "distribution3"
			RadioButton { value: "binom";		label: qsTr("Binomial"); id: binom3; checked: false }
			RadioButton { value: "hypergeom";	label: qsTr("Hypergeometric"); id: hypergeom3 }
			RadioButton { value: "poisson";		label: qsTr("Poisson"); id: poisson3 }
		}

		CheckBox{ label: qsTr("Show summary (third plan)"); name: "showSummary3"}
	}
}