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

RadioButtonGroup
{
	title: qsTr("Prior Distribution (Beta)")
	property string suffix: ""
	name: "prior" + suffix
	RadioButton { value: "impartial"; label: qsTr("Impartial"); checked: true }
	RadioButton { value: "uniform"; label: qsTr("Uniform") }
	RadioButton 
	{ 
		value: "custom"; label: qsTr("Custom")
		DoubleField{ name: "alpha" + suffix; label: qsTr("\u03B1"); defaultValue: 1; min: 0 }
		DoubleField { name: "beta" + suffix; label: qsTr("\u03B2"); defaultValue: 1; min: 0 }			
	}
}