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

Group
{
    // Layout.columnSpan: 2
    TextField { name: "sampleSizeMult"; label: qsTr("Sample sizes (n1,n2,...): "); inputType: "integerArray"; fieldWidth: 60 }
    TextField { name: "acceptNumberMult"; label: qsTr("Acceptance numbers (c1,c2,...): "); inputType: "integerArray"; fieldWidth: 60 }
    TextField { name: "rejectNumberMult"; label: qsTr("Rejection numbers (r1,r2,...): "); inputType: "integerArray"; fieldWidth: 60 }
}