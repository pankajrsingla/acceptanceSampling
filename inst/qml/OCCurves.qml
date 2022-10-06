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
	Section
	{
		title: qsTr("First OC Curve")

		Common.PlanSingle
		{
			// framework:	form.framework
		}
		
		Common.Distribution
		{
			// something
		}

		CheckBox{ name: "showSummary1"; label: qsTr("Show summary (first plan)") }
	}

	Section
	{
		title: qsTr("Second OC Curve")
		// enabled: acceptNumber1.value > 0 && rejectNumber1.value > 0

		Common.PlanSingle
		{
			// framework:	form.framework
		}
		
		Common.Distribution
		{
			// something
		}

		// Group
		// {
		// 	Layout.columnSpan: 2
		// 	IntegerField { name: "sampleSize2"; label: qsTr("Sample size (n): "); defaultValue: 1; min: 1 }
		// 	IntegerField { id: acceptNumber2; name: "acceptNumber2"; label: qsTr("Acceptance number (c): "); defaultValue: 0 }
		// 	IntegerField { id: rejectNumber2; name: "rejectNumber2"; label: qsTr("Rejection number (r): "); defaultValue: 0 }
		// }
	
		// RadioButtonGroup
		// {
		// 	title: qsTr("Distribution")
		// 	name: "distribution2"
		// 	RadioButton { value: "binom"; label: qsTr("Binomial"); checked: false }
		// 	Group
		// 	{
		// 		RadioButton { value: "hypergeom"; label: qsTr("Hypergeometric"); id: hypergeom2 }
		// 		IntegerField { name: "lotSize2"; label: qsTr("Lot size (N): "); defaultValue: 1; min: 1; enabled: hypergeom2.checked }
		// 	}
		// 	RadioButton { value: "poisson"; label: qsTr("Poisson") }
		// }

		CheckBox{ name: "showSummary2"; label: qsTr("Show summary (second plan)") }
	}

	Section
	{
		title: qsTr("Third OC Curve")
		// enabled: acceptNumber2.value > 0 && rejectNumber2.value > 0

		Common.PlanSingle
		{
			// framework:	form.framework
		}
		
		Common.Distribution
		{
			// something
		}

		// Group
		// {
		// 	Layout.columnSpan: 2
		// 	IntegerField { name: "sampleSize3"; label: qsTr("Sample size (n): "); defaultValue: 1; min: 1 }
		// 	IntegerField { name: "acceptNumber3"; label: qsTr("Acceptance number (c): "); defaultValue: 0 }
		// 	IntegerField { name: "rejectNumber3"; label: qsTr("Rejection number (r): "); defaultValue: 0 }
		// }

		// RadioButtonGroup
		// {
		// 	title: qsTr("Distribution")
		// 	name: "distribution3"
		// 	RadioButton { value: "binom"; label: qsTr("Binomial"); checked: false }
		// 	Group
		// 	{
		// 		RadioButton { value: "hypergeom"; label: qsTr("Hypergeometric"); id: hypergeom3 }
		// 		IntegerField { name: "lotSize3"; label: qsTr("Lot size (N): "); defaultValue: 1; min: 1; enabled: hypergeom3.checked }
		// 	}
		// 	RadioButton { value: "poisson"; label: qsTr("Poisson") }
		// }

		CheckBox{ name: "showSummary3"; label: qsTr("Show summary (third plan)") }
	}

	Section
	{
		title: qsTr("Multiple Sampling Plan")

		Common.PlanMultiple
		{
			// something
		}

		Common.Distribution
		{
			// something
		}

		// Group
		// {
		// 	Layout.columnSpan: 2
		// 	TextField { name: "sampleSize_mult"; label: qsTr("Sample sizes (n1,n2,...): "); inputType: "integerArray"; fieldWidth: 60 }
		// 	TextField { name: "acceptNumber_mult"; label: qsTr("Acceptance numbers (c1,c2,...): "); inputType: "integerArray"; fieldWidth: 60 }
		// 	TextField { name: "rejectNumber_mult"; label: qsTr("Rejection numbers (r1,r2,...): "); inputType: "integerArray"; fieldWidth: 60 }
		// }

		// RadioButtonGroup
		// {
		// 	title: qsTr("Distribution")
		// 	name: "distribution_mult"
		// 	RadioButton { value: "binom"; label: qsTr("Binomial"); checked: false }
		// 	Group
		// 	{
		// 		RadioButton { value: "hypergeom"; label: qsTr("Hypergeometric"); id: hypergeom_mult }
		// 		IntegerField { name: "lotSize_mult"; label: qsTr("Lot size (N): "); defaultValue: 1; min: 1; enabled: hypergeom_mult.checked }
		// 	}
		// 	RadioButton { value: "poisson"; label: qsTr("Poisson") }
		// }

		CheckBox{ name: "showSummary_mult"; label: qsTr("Show summary") }
	}
}