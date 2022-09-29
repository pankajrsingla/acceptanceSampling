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
		title: qsTr("First OC Curve")
		
		FormulaField { name: "lotSize1"; label: qsTr("Lot size (N): "); value: "100" ; min: 1; max: Infinity; Layout.columnSpan: 2; enabled: hypergeom1.checked }
		FormulaField { name: "sampleSize1"; label: qsTr("Sample size (n): "); value: "10" ; min: 1; max: Infinity; Layout.columnSpan: 2 }
		FormulaField { name: "acceptNumber1"; label: qsTr("Acceptance number (c): "); value: "4" ; min: 0; max: Infinity; Layout.columnSpan: 2 }
		FormulaField { name: "rejectNumber1"; label: qsTr("Rejection number (r): "); value: "5" ; min: 0; max: Infinity; Layout.columnSpan: 2 }

		RadioButtonGroup
		{
			title: qsTr("Distribution")
			name: "distribution1"
			RadioButton { value: "binom";		label: qsTr("Binomial"); id: binom1; checked: true	}
			RadioButton { value: "hypergeom";	label: qsTr("Hypergeometric"); id: hypergeom1		}
			RadioButton { value: "poisson";		label: qsTr("Poisson"); id: poisson1					}
		}
	}
	Section
	{
		title: qsTr("Second OC Curve")

		FormulaField { name: "lotSize2"; label: qsTr("Lot size (N): "); value: "0" ; min: 1; max: Infinity; Layout.columnSpan: 2; parseDefaultValue: false; enabled: hypergeom2.checked }
		FormulaField { name: "sampleSize2"; label: qsTr("Sample size (n): "); value: "0" ; min: 1; max: Infinity; Layout.columnSpan: 2; parseDefaultValue: false }
		FormulaField { name: "acceptNumber2"; label: qsTr("Acceptance number (c): "); value: "0" ; min: 0; max: Infinity; Layout.columnSpan: 2; parseDefaultValue: false }
		FormulaField { name: "rejectNumber2"; label: qsTr("Rejection number (r): "); value: "0" ; min: 0; max: Infinity; Layout.columnSpan: 2; parseDefaultValue: false }

		RadioButtonGroup
		{
			title: qsTr("Distribution")
			name: "distribution2"
			RadioButton { value: "binom";		label: qsTr("Binomial"); id: binom2; checked: true	}
			RadioButton { value: "hypergeom";	label: qsTr("Hypergeometric"); id: hypergeom2		}
			RadioButton { value: "poisson";		label: qsTr("Poisson"); id: poisson2					}
		}
	}

	Section
	{
		title: qsTr("Third OC Curve")

		FormulaField { name: "lotSize3"; label: qsTr("Lot size (N): "); value: "0" ; min: 1; max: Infinity; Layout.columnSpan: 2; parseDefaultValue: false; enabled: hypergeom3.checked }
		FormulaField { name: "sampleSize3"; label: qsTr("Sample size (n): "); value: "0" ; min: 1; max: Infinity; Layout.columnSpan: 2; parseDefaultValue: false }
		FormulaField { name: "acceptNumber3"; label: qsTr("Acceptance number (c): "); value: "0" ; min: 0; max: Infinity; Layout.columnSpan: 2; parseDefaultValue: false }
		FormulaField { name: "rejectNumber3"; label: qsTr("Rejection number (r): "); value: "0" ; min: 0; max: Infinity; Layout.columnSpan: 2; parseDefaultValue: false }

		RadioButtonGroup
		{
			title: qsTr("Distribution")
			name: "distribution3"
			RadioButton { value: "binom";		label: qsTr("Binomial"); id: binom3; checked: true	}
			RadioButton { value: "hypergeom";	label: qsTr("Hypergeometric"); id: hypergeom3		}
			RadioButton { value: "poisson";		label: qsTr("Poisson"); id: poisson3					}
		}
	}
}