| package |
package := Package name: 'tp'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Clinica;
	add: #ConsultasClinicas;
	add: #MinutaActividad;
	add: #ObraSocial;
	add: #Paciente;
	add: #Persona;
	add: #Profesional;
	add: #ReunionesGrupales;
	add: #SesionesDeGimnasia;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Clinica
	instanceVariableNames: 'pacientes profesionales consultasClinicas reunionesGrupales sesionesGimnasias obrasSociales minutasDeActividad'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #ConsultasClinicas
	instanceVariableNames: 'tipoConsulta descripcion paciente profesional fecha'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #MinutaActividad
	instanceVariableNames: 'fechaAsistencia paciente pesoPaciente profesional actividad id costo'
	classVariableNames: 'UltimoCodigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #ObraSocial
	instanceVariableNames: 'porcentajeServicio nombre codigo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Persona
	instanceVariableNames: 'tipoDni numeroDni apellido nombre direccion telefono'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #ReunionesGrupales
	instanceVariableNames: 'descripcion costo coordinador paciente id fecha'
	classVariableNames: 'UltimoCodigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #SesionesDeGimnasia
	instanceVariableNames: 'descripcion fecha pacientes id'
	classVariableNames: 'Costo UltimoCodigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Persona subclass: #Paciente
	instanceVariableNames: 'obraSocial codigoPaciente'
	classVariableNames: 'UltimoCodigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Persona subclass: #Profesional
	instanceVariableNames: 'matricula tarifaConsulta tarifaReunion tipoProfesion'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Clinica guid: (GUID fromString: '{6fef658a-d7e7-4e02-82dc-ec2aa5e6d6d8}')!
Clinica comment: ''!
!Clinica categoriesForClass!Kernel-Objects! !
!Clinica methodsFor!

agregarPacienteReunion
	| profe msj eleccion minutaPorRegistrar pac costoMinuta |
	costoMinuta := 0.
	eleccion := ''.
	msj := ''.
	profe := ''.
	minutaPorRegistrar := MinutaActividad new.
	reunionesGrupales do: 
			[:reunion |
			Date today <= reunion fecha
				ifTrue: 
					[profe := self busquedaProfesionalDni: reunion coordinador numeroDni.
					msj := msj , 'Profesional: ' , profe nombre printString , ' ' , profe apellido printString , ' ID: '
								, reunion id , ' Fecha: '
								, reunion fecha printString , String cr]].
	MessageBox notify: msj , String cr , 'seleccione el id: '.
	eleccion := Prompter prompt: 'seleccione el id: ' caption: 'Agregando paciente...'.
	msj := ''.
	reunionesGrupales do: 
			[:reunion |
			reunion id = eleccion
				ifTrue: 
					[pac := self muestreoPaciente.
					costoMinuta := reunion costo + reunion coordinador tarifaReunion.
					pac = 0
						ifFalse: 
							[reunion agregarPaciente: pac.
							minutaPorRegistrar actividad: reunion.
							minutaPorRegistrar paciente: pac.
							MinutaActividad incrementarCod.
							minutaPorRegistrar fechaAsistencia: reunion fecha.
							minutaPorRegistrar pesoPaciente: 0.
							minutaPorRegistrar costo: costoMinuta.
							minutaPorRegistrar id: MinutaActividad ultimoCodigo.
							minutasDeActividad add: minutaPorRegistrar]]]!

agregarPacienteSesionG
	| msj eleccion dniSeleccionado pac minutaPorRegistrar costoMinuta |
	costoMinuta := SesionesDeGimnasia costo.
	eleccion := ''.
	dniSeleccionado := 0.
	msj := ''.
	minutaPorRegistrar := MinutaActividad new.
	sesionesGimnasias do: 
			[:sesion |
			Date today <= sesion fecha
				ifTrue: [msj := msj , ' ID: ' , sesion id , ' Fecha: ' , sesion fecha printString , String cr]].
	MessageBox notify: msj , String cr , 'seleccione el id: ' caption: ' Sesiones disponibles.'.
	eleccion := Prompter prompt: 'seleccione el id: '.
	msj := ''.
	sesionesGimnasias do: 
			[:sesion |
			sesion id = eleccion
				ifTrue: 
					[pacientes do: 
							[:pacien |
							msj := msj , 'Nombre paciente: ' , pacien nombre , ' dni:  ' , pacien numeroDni printString
										, String cr].
					MessageBox notify: msj , String cr , 'seleccione el dni: '.
					dniSeleccionado := (Prompter prompt: 'seleccione el dni ') asNumber.
					pac := pacientes detect: [:each | each numeroDni = dniSeleccionado] ifNone: [nil].
					pac isNil
						ifTrue: [MessageBox notify: 'NO EXISTE EL DNI']
						ifFalse: 
							[sesion agregarPaciente: pac.
							MessageBox notify: 'Se agrego EXITOSAMENTE'.
							minutaPorRegistrar actividad: sesion.
							minutaPorRegistrar paciente: pac.
							MinutaActividad incrementarCod.
							minutaPorRegistrar fechaAsistencia: sesion fecha.
							minutaPorRegistrar pesoPaciente: 0.
							minutaPorRegistrar costo: costoMinuta.
							minutaPorRegistrar id: MinutaActividad ultimoCodigo.
							minutasDeActividad add: minutaPorRegistrar]]]!

asignarObraSocial
	| msj credencial codigo |
	codigo := ''.
	msj := ''.
	obrasSociales do: 
			[:obra |
			msj := msj , 'Nombre ' , obra nombre printString , '     Codigo: ' , obra codigo printString
						, String cr].
	MessageBox notify: msj , String cr , 'seleccione el codigo: ' caption:'Obras sociales disponibles.'.
	codigo := (Prompter
				prompt: 'Si tiene obra social ingrese el numero de la misma, de lo contrario deje el campo vacio: '
					caption: 'Ingresando numero de obra social...') asNumber.
	credencial := obrasSociales detect: [:each | each codigo = codigo 	] ifNone: [nil].
	credencial isNil
		ifTrue: 
			[codigo = 0
				ifTrue: 
					[MessageBox notify:'Paciente agregado exitosamente'caption:'Carga paciente.' .
					^0]
				ifFalse: [MessageBox warning: 'Codigo inexistente' caption: 'Falla en la carga de paciente.']]
		ifFalse: 
			[MessageBox notify: 'Paciente agregado exitosamente' caption:'Carga paciente.'.
			^credencial]!

busquedaProfesionalDni: unDni
	profesionales do: [:profe | profe numeroDni = unDni ifTrue: [^profe]]!

informeParaObraSocial
	| fecha1 fecha2 os msj total |
	msj := ''.
	total := 0.
	os := self muestreoObraSocial.
	os = 0
		ifFalse: 
			[fecha1 := Date fromString: (Prompter prompt: 'Ingrese la fecha de inicio del rango DD/MM/YYYY').
			fecha2 := Date fromString: (Prompter prompt: 'Ingrese la fecha de fin del rango DD/MM/YYYY').
			minutasDeActividad do: 
					[:min |
					min pesoPaciente = 0
						ifFalse: 
							[min fechaAsistencia <= fecha2 & (min fechaAsistencia >= fecha1) & (min paciente obraSocial = os)
								ifTrue: 
									[msj := msj , 'Nombre: ' , min paciente nombre , ' costo: ' , min costo printString , String cr.
									total := total + min costo]]].
			msj := msj , String cr , 'El costo total es de: ' , total printString
						, ' la obra social debe cubrir: ' , (total * os porcentajeServicio / 100) printString.
			MessageBox notify: msj]!

informeReunion
	| msj msj2 eleccion costoTotal |
	eleccion := ''.
	msj := ''.
	reunionesGrupales do: 
			[:reunion |
			msj := msj , 'Nombre prof: ' , reunion coordinador nombre , '  ' , reunion coordinador apellido
						, ' Id: ' , reunion id
						, ' Fecha: ' , reunion fecha printString
						, String cr].
	MessageBox notify: msj , String cr , 'seleccione el id: ' caption: 'Reuniones disponibles'.
	eleccion := Prompter prompt: 'seleccione el id: ' caption: 'Ingresando id de reunion deseada...'.
	msj := ''.
	reunionesGrupales do: 
			[:reunion |
			reunion id = eleccion
				ifTrue: 
					[pacientes do: 
							[:pacien |
							msj := msj , 'Paciente: ' , pacien nombre , ' ' , pacien apellido , ' dni:  '
										, pacien numeroDni printString , String cr].
					costoTotal := reunion costo + reunion coordinador tarifaReunion.
					msj2 := 'Profesional : ' , reunion coordinador nombre , ' ' , reunion coordinador apellido
								, String cr , 'Costo total:'
								, costoTotal printString , String cr
								, 'Descripcion:' , reunion descripcion
								, String cr , reunion fecha printString
								, String cr , msj
								, String cr.
					MessageBox notify: msj2 caption: 'Informe de reunion.']]!

informeSesion
	| msj msj2 eleccion  |
	eleccion := ''.
	msj := ''.
	sesionesGimnasias do: [:sesion | msj := msj , ' Id: ' , sesion id , ' Fecha: ' , sesion fecha printString , String cr].
	MessageBox notify: msj , String cr , 'seleccione el id: ' caption: 'Sesiones disponibles.'.
	eleccion := Prompter prompt: 'seleccione el id: ' caption: 'Ingresando id de la sesion deseada...'.
	msj := ''.
	sesionesGimnasias do: 
			[:sesion |
			sesion id = eleccion
				ifTrue: 
					[pacientes do: 
							[:pacien |
							msj := msj , 'Paciente: ' , pacien nombre , ' ' , pacien apellido , ' dni:  '
										, pacien numeroDni printString , String cr].
					
					msj2 := 'Costo total:' , SesionesDeGimnasia costo printString , String cr , 'Descripción:' , sesion descripcion
								, String cr , sesion fecha printString
								, String cr , msj
								, String cr.
					MessageBox notify: msj2 caption: 'Informe de sesión.']]!

inicializar
	| x |
	x := Prompter prompt: 'Ingrese el costo para las sesiones de gimnasia: '
				caption: 'Ingresando costo de la sesión...'.
	pacientes := OrderedCollection new.
	profesionales := OrderedCollection new.
	consultasClinicas := OrderedCollection new.
	reunionesGrupales := OrderedCollection new.
	sesionesGimnasias := OrderedCollection new.
	minutasDeActividad := OrderedCollection new.
	obrasSociales := OrderedCollection new.
	ReunionesGrupales inicializarCodigo.
	SesionesDeGimnasia inicializarCodigo.
	MinutaActividad inicializarCodigo.
	SesionesDeGimnasia costo: x.
	Paciente inicializarCodigo!

menu
	| op cantidad |
	cantidad := 0.
	op := 13.
	[op = 0] whileFalse: 
			[MessageBox
				notify: 'MENU:
01- Alta obra social.
02- Alta profesional.
03- Alta paciente.
04- Alta reunion.
05- Alta sesion.
06- Alta consulta.
07- Registrar minuta de actividad.
08- Agregar paciente a reunion.
09- Agregar paciente a sesion de gimnasia.
10- Generar deuda de obra social.
11- Generar informe de reuniones.
12- Generar informe de sesiones de gimnasia.
00- Salir.
'.
			op := (Prompter prompt: 'Ingrese opción:') asNumber asInteger.
			op = 1 ifTrue: [self regObraSocial].
			op = 2 ifTrue: [self regProfesional].
			op = 3
				ifTrue: 
					[cantidad := obrasSociales size.
					cantidad = 0
						ifTrue: 
							[MessageBox warning: 'No hay obras sociales registradas, para continuar ingrese al menos una. ']
						ifFalse: [self regPaciente]].
			op = 4
				ifTrue: 
					[cantidad := profesionales size.
					cantidad = 0
						ifTrue: [MessageBox warning: 'No hay profesionales registrados, para continuar ingrese al menos uno. ']
						ifFalse: [self regReunion]].
			op = 5
				ifTrue: 
					[cantidad := profesionales size.
					cantidad = 0
						ifTrue: [MessageBox warning: 'No hay profesionales registrados, para continuar ingrese al menos uno. ']
						ifFalse: 
							[cantidad := pacientes size.
							cantidad = 0
								ifTrue: [MessageBox warning: 'No hay pacientes registrados, para continuar ingrese al menos uno. ']
								ifFalse: [self regPaciente]]].
			op = 6
				ifTrue: 
					[cantidad := profesionales size.
					cantidad = 0
						ifTrue: [MessageBox warning: 'No hay profesionales registrados, para continuar ingrese al menos uno. ']
						ifFalse: 
							[cantidad := pacientes size.
							cantidad = 0
								ifTrue: [MessageBox warning: 'No hay pacientes registrados, para continuar ingrese al menos uno. ']
								ifFalse: [self regPaciente]]].
			op = 7
				ifTrue: 
					[cantidad := minutasDeActividad size.
					cantidad = 0
						ifTrue: 
							[MessageBox
								warning: 'No hay actividades registradas/asignadas, para continuar ingrese al menos una. ']
						ifFalse: [self regMinutaActividad ]].
			op = 8
				ifTrue: 
					[cantidad := pacientes size.
					cantidad = 0
						ifTrue: [MessageBox warning: 'No hay pacientes registrados, para continuar ingrese al menos uno. ']
						ifFalse: 
							[cantidad := reunionesGrupales size.
							cantidad = 0
								ifTrue: [MessageBox warning: 'No hay reuniones registradas, para continuar ingrese al menos una. ']
								ifFalse: [self agregarPacienteReunion]]].
			op = 9
				ifTrue: 
					[cantidad := pacientes size.
					cantidad = 0
						ifTrue: [MessageBox warning: 'No hay pacientes registrados, para continuar ingrese al menos uno. ']
						ifFalse: 
							[cantidad := sesionesGimnasias size.
							cantidad = 0
								ifTrue: 
									[MessageBox warning: 'No hay sesiones de gimnasia registradas, para continuar ingrese al menos una. ']
								ifFalse: [self agregarPacienteSesionG]]].
			op = 10
				ifTrue: 
					[cantidad := obrasSociales size.
					cantidad = 0
						ifTrue: 
							[MessageBox warning: 'No hay obras sociales registradas, para continuar ingrese al menos una. ']
						ifFalse: [self informeParaObraSocial]].
			op = 11
				ifTrue: 
					[cantidad := reunionesGrupales size.
					cantidad = 0
						ifTrue: [MessageBox warning: 'No hay reuniones registradas, para continuar ingrese al menos una. ']
						ifFalse: [self informeReunion]].
			op = 12
				ifTrue: 
					[cantidad := sesionesGimnasias size.
					cantidad = 0
						ifTrue: 
							[MessageBox warning: 'No hay sesiones de gimnasia registradas, para continuar ingrese al menos una. ']
						ifFalse: [self informeSesion]]]!

muestreoObraSocial
	| msj credencial codigo |
	codigo := ''.
	msj := ''.
	obrasSociales do: 
			[:obra |
			msj := msj , 'Nombre ' , obra nombre printString , '     Codigo: ' , obra codigo printString
						, String cr].
	MessageBox notify: msj , String cr , 'seleccione el codigo: ' caption: 'Obras sociales disponibles.'.
	codigo := (Prompter
				prompt: 'Si tiene obra social ingrese el numero de la misma, de lo contrario deje el campo vacio: '
				caption: 'Ingresando numero de obra social...') asNumber.
	credencial := obrasSociales detect: [:each | each codigo = codigo] ifNone: [nil].
	credencial isNil
		ifTrue: 
			[MessageBox warning: 'NO EXISTE LA OBRA SOCIAL'.
			^0]
		ifFalse: 
			[MessageBox notify: 'Se seleccionó la Obra social'.
			^credencial]!

muestreoPaciente
	| msj dniSeleccionado pac |
	msj := ''.
	pacientes do: 
			[:pacien |
			msj := msj , 'Nombre paciente: ' , pacien nombre , ' dni:  ' , pacien numeroDni printString
						, String cr].
	MessageBox notify: msj , String cr , 'seleccione el dni: '.
	dniSeleccionado := (Prompter prompt: 'seleccione el dni ') asNumber.
	pac := pacientes detect: [:each | each numeroDni = dniSeleccionado] ifNone: [nil].
	pac isNil
		ifTrue: 
			[MessageBox warning: 'NO EXISTE EL DNI'.
			^0]
		ifFalse: 
			[MessageBox notify: 'Se seleccionó el paciente'.
			^pac]!

muestreoProfesional
	| msj dniSeleccionado pro |
	msj := 'Nombre: ' , String cr.
	profesionales do: 
			[:profe |
			msj := msj , profe nombre , ' ' , profe apellido , ' dni: ' , profe numeroDni printString
						, profe tipoProfesion , String cr].
	MessageBox notify: msj , String cr , 'seleccione el dni: '.
	dniSeleccionado := (Prompter prompt: 'seleccione el dni ') asNumber.
	pro := profesionales detect: [:each | each numeroDni = dniSeleccionado] ifNone: [nil].
	pro isNil
		ifTrue: 
			[MessageBox warning: 'NO EXISTE EL DNI'.
			^0]
		ifFalse: 
			[MessageBox notify: 'Se seleccionó el profesional'.
			^pro]!

regConsultasClinicas
	| consultaClinicaPorIngresar profe pac minutaPorRegistrar costoMinuta |
	costoMinuta := 0.
	profe := self muestreoProfesional.
	minutaPorRegistrar := MinutaActividad new.
	profe = 0
		ifFalse: 
			[pac := self muestreoPaciente.
			pac = 0
				ifFalse: 
					[consultaClinicaPorIngresar := ConsultasClinicas new.
					consultaClinicaPorIngresar cargaDatos.
					consultaClinicaPorIngresar paciente: pac.
					consultaClinicaPorIngresar profesional: profe.
					consultasClinicas add: consultaClinicaPorIngresar.
					MessageBox notify: 'Se ingreso exitosamente la consulta'.
					minutaPorRegistrar actividad: consultaClinicaPorIngresar.
					minutaPorRegistrar paciente: pac.
					MinutaActividad incrementarCod.
					minutaPorRegistrar fechaAsistencia: consultaClinicaPorIngresar fecha.
					minutaPorRegistrar pesoPaciente: 0.
					minutaPorRegistrar id: MinutaActividad ultimoCodigo.
					costoMinuta := profe tarifaConsulta.
					minutaPorRegistrar costo: costoMinuta.
					minutasDeActividad add: minutaPorRegistrar]]!

regMinutaActividad
	| idSeleccionado minutaPorIngresar profe pac msj |
	msj := ''.
	MessageBox notify: 'Ingrese DNI de paciente a registrar en la minuta de actividad'.
	pac := self muestreoPaciente.
	pac = 0
		ifFalse: 
			[minutasDeActividad do: 
					[:minun |
					minun actividad fecha = Date today & (minun paciente = pac) & (minun pesoPaciente = 0)
						ifTrue: 
							[msj := msj , 'ID:  ' , minun id printString , '   ' , minun paciente nombre , '  dni:  '
										, minun paciente numeroDni printString , '    '
										, minun actividad class printString , String cr]].
			MessageBox notify: msj , String cr , 'Seleccione el ID '.
			idSeleccionado := (Prompter prompt: 'Seleccione el ID ') asNumber].
	minutaPorIngresar := minutasDeActividad detect: [:each | each id = idSeleccionado] ifNone: [nil].
	minutaPorIngresar pesoPaciente: (Prompter prompt: 'Ingrese el peso del paciente.') asNumber.
	profe := MessageBox confirm: 'Desea ingresar un profesional de asistencia?'.
	profe = true
		ifTrue: 
			[profe := self muestreoProfesional.
			profe = 0
				ifFalse: 
					[minutaPorIngresar profesional: profe.
					consultasClinicas add: minutaPorIngresar.
					MessageBox notify: 'Se ingreso exitosamente la minuta']]
		ifFalse: 
			[minutaPorIngresar profesional: 0.
			consultasClinicas add: minutaPorIngresar.
			MessageBox notify: 'Se ingreso exitosamente la minuta']!

regObraSocial
	| obraSocialPorIngresar |
	obraSocialPorIngresar := ObraSocial new.
	obraSocialPorIngresar cargaDatos.
	obrasSociales add: obraSocialPorIngresar!

regPaciente
	|pacientePorIngresar|
	pacientePorIngresar:=Paciente new.
	pacientePorIngresar cargaDatos.
	pacientePorIngresar obraSocial: self asignarObraSocial.
	pacientes add: pacientePorIngresar.!

regProfesional
	| profesionalPorIngresar |
	profesionalPorIngresar := Profesional new.
	profesionalPorIngresar cargaDatos.
	profesionales add: profesionalPorIngresar!

regReunion
	| reunionPorIngresar profe |
	reunionPorIngresar := ReunionesGrupales new.
	reunionPorIngresar cargaDatos.
	profe := self muestreoProfesional.
	reunionPorIngresar coordinador: profe.
	profe = 0 ifFalse: [reunionesGrupales add: reunionPorIngresar]!

regSesionGimnasia
	| sesionGimnasiaPorIngresar |
	sesionGimnasiaPorIngresar := SesionesDeGimnasia new.
	sesionGimnasiaPorIngresar cargaDatos.
	sesionesGimnasias add: sesionGimnasiaPorIngresar! !
!Clinica categoriesForMethods!
agregarPacienteReunion!public! !
agregarPacienteSesionG!public! !
asignarObraSocial!public! !
busquedaProfesionalDni:!public! !
informeParaObraSocial!public! !
informeReunion!public! !
informeSesion!public! !
inicializar!public! !
menu!public! !
muestreoObraSocial!public! !
muestreoPaciente!public! !
muestreoProfesional!public! !
regConsultasClinicas!public! !
regMinutaActividad!public! !
regObraSocial!public! !
regPaciente!public! !
regProfesional!public! !
regReunion!public! !
regSesionGimnasia!public! !
!

ConsultasClinicas guid: (GUID fromString: '{385b1d79-94af-4f77-9e09-306278467100}')!
ConsultasClinicas comment: ''!
!ConsultasClinicas categoriesForClass!Kernel-Objects! !
!ConsultasClinicas methodsFor!

cargaDatos
	MessageBox notify: 'A continuacion ingrese los datos solicitados. ' caption: ''.
	descripcion := (Prompter prompt: 'Ingrese la descripcion '
				caption: 'Ingresando datos de la consulta...') asString.
	tipoConsulta := (Prompter prompt: 'Ingrese el tipo de consulta: '
				caption: 'Ingresando datos de la consulta...') asString.
	fecha := Date fromString: (Prompter prompt: 'Ingrese la fecha de la reunion DD/MM/YYYY'
						caption: 'Ingresando datos de la Consulta Clinica...')!

descripcion
	^descripcion!

descripcion: anObject
	descripcion := anObject!

fecha
	^fecha!

fecha: anObject
	fecha := anObject!

paciente
	^paciente!

paciente: anObject
	paciente := anObject!

profesional
	^profesional!

profesional: anObject
	profesional := anObject!

tipoConsulta
	^tipoConsulta!

tipoConsulta: anObject
	tipoConsulta := anObject! !
!ConsultasClinicas categoriesForMethods!
cargaDatos!public! !
descripcion!accessing!private! !
descripcion:!accessing!private! !
fecha!accessing!private! !
fecha:!accessing!private! !
paciente!accessing!private! !
paciente:!accessing!private! !
profesional!accessing!private! !
profesional:!accessing!private! !
tipoConsulta!accessing!private! !
tipoConsulta:!accessing!private! !
!

MinutaActividad guid: (GUID fromString: '{42727b85-6a74-422f-9583-9a67c08a3044}')!
MinutaActividad comment: ''!
!MinutaActividad categoriesForClass!Unclassified! !
!MinutaActividad methodsFor!

actividad
	^actividad!

actividad: anObject
	actividad := anObject!

cargaDatos
	MessageBox notify: 'A continuacion ingrese los datos solicitados. '
		caption: 'Carga datos Minuta Actividad'.
	pesoPaciente := (Prompter prompt: 'Ingrese peso actual del paciente en kg: ') asNumber.
	fechaAsistencia := Date today!

costo
	^costo!

costo: anObject
	costo := anObject!

fechaAsistencia
	^fechaAsistencia!

fechaAsistencia: anObject
	fechaAsistencia := anObject!

id
	^id!

id: anObject
	id := anObject!

paciente
	^paciente!

paciente: anObject
	paciente := anObject!

pesoPaciente
	^pesoPaciente!

pesoPaciente: anObject
	pesoPaciente := anObject!

profesional
	^profesional!

profesional: anObject
	profesional := anObject! !
!MinutaActividad categoriesForMethods!
actividad!accessing!private! !
actividad:!accessing!private! !
cargaDatos!public! !
costo!accessing!private! !
costo:!accessing!private! !
fechaAsistencia!accessing!private! !
fechaAsistencia:!accessing!private! !
id!accessing!private! !
id:!accessing!private! !
paciente!accessing!private! !
paciente:!accessing!private! !
pesoPaciente!accessing!private! !
pesoPaciente:!accessing!private! !
profesional!accessing!private! !
profesional:!accessing!private! !
!

!MinutaActividad class methodsFor!

incrementarCod
	UltimoCodigo := UltimoCodigo + 1!

inicializarCodigo
	UltimoCodigo:=0.!

ultimoCodigo
	^UltimoCodigo! !
!MinutaActividad class categoriesForMethods!
incrementarCod!public! !
inicializarCodigo!public! !
ultimoCodigo!public! !
!

ObraSocial guid: (GUID fromString: '{1bd18e35-c8bd-418b-bd00-5b5b1c26570f}')!
ObraSocial comment: ''!
!ObraSocial categoriesForClass!Kernel-Objects! !
!ObraSocial methodsFor!

cargaDatos
	MessageBox notify: 'A continuacion ingrese los datos solicitados. ' caption: 'Cargue datos de la obra social. '.
	codigo := (Prompter prompt: 'Ingrese el codigo: ' caption: 'Ingresando datos de obra social...') asNumber .
	nombre := (Prompter prompt: 'Ingrese nombre: ' caption: 'Ingresando datos de obra social...') asString.
	porcentajeServicio := (Prompter prompt: 'Ingrese el porcentaje de descuento: ' caption: 'Ingresando datos de obra social...') asNumber.
!

codigo
	^codigo!

codigo: anObject
	codigo := anObject!

nombre
	^nombre!

nombre: anObject
	nombre := anObject!

porcentajeServicio
	^porcentajeServicio!

porcentajeServicio: anObject
	porcentajeServicio := anObject! !
!ObraSocial categoriesForMethods!
cargaDatos!public! !
codigo!accessing!private! !
codigo:!accessing!private! !
nombre!accessing!private! !
nombre:!accessing!private! !
porcentajeServicio!accessing!private! !
porcentajeServicio:!accessing!private! !
!

Persona guid: (GUID fromString: '{143d9298-34c5-4a36-9468-284a5bd94054}')!
Persona comment: ''!
!Persona categoriesForClass!Kernel-Objects! !
!Persona methodsFor!

apellido
^apellido!

apellido: unString
apellido:= unString!

cargaDatos

MessageBox notify: ('A continuacion ingrese los datos solicitados. ')asString.

tipoDni := (Prompter prompt: 'Ingrese tipo DNI: ' ) asString.
numeroDni := (Prompter prompt: 'Ingrese numero DNI: ' ) asNumber.
apellido := (Prompter prompt: 'Ingrese apellido: ' ) asString.
nombre := (Prompter prompt: 'Ingrese nombre' ) asString.
direccion := (Prompter prompt: 'Ingrese direccion' ) asString.
telefono := (Prompter prompt: 'Ingrese numero telefono: ' ) asNumber.

!

direccion
^direccion!

direccion: unString
direccion:=unString!

nombre
^nombre!

nombre: unString
nombre:=unString!

numeroDni
^numeroDni!

numeroDni: unNumero
numeroDni:= unNumero!

ObtenerNombre
^ nombre!

telefono
^telefono!

telefono: unNumero
telefono:=unNumero!

tipoDni
^ tipoDni!

tipoDni: unString
tipoDni := unString ! !
!Persona categoriesForMethods!
apellido!public! !
apellido:!public! !
cargaDatos!public! !
direccion!public! !
direccion:!public! !
nombre!public! !
nombre:!public! !
numeroDni!public! !
numeroDni:!public! !
ObtenerNombre!public! !
telefono!public! !
telefono:!public! !
tipoDni!public! !
tipoDni:!public! !
!

ReunionesGrupales guid: (GUID fromString: '{a0a521ec-6769-40dc-9912-548352973d1a}')!
ReunionesGrupales comment: ''!
!ReunionesGrupales categoriesForClass!Kernel-Objects! !
!ReunionesGrupales methodsFor!

agregarPaciente: unPaciente
	paciente add: unPaciente!

cargaDatos
	self inicializarColeccion.
	MessageBox notify: 'A continuacion ingrese los datos solicitados. ' asString.
	descripcion := (Prompter prompt: 'Ingrese la descripción: ' caption: 'Ingresando datos de la reunion grupal...') asString.
	costo := (Prompter prompt: 'Ingrese eI costo: ' caption: 'Ingresando datos de la reunion grupal...') asNumber.
	ReunionesGrupales incrementarCodReunion.
	id := UltimoCodigo printString.
	fecha := Date fromString: (Prompter prompt: 'Ingrese la fecha de la reunion DD/MM/YYYY' caption: 'Ingresando datos de la reunion grupal...')!

coordinador
	^coordinador!

coordinador: anObject
	coordinador := anObject!

costo
	^costo!

costo: anObject
	costo := anObject!

descripcion
	^descripcion!

descripcion: anObject
	descripcion := anObject!

fecha
	^fecha!

fecha: anObject
	fecha := anObject!

id
	^id!

id: anObject
	id := anObject!

inicializarColeccion
	paciente := OrderedCollection new! !
!ReunionesGrupales categoriesForMethods!
agregarPaciente:!public! !
cargaDatos!public! !
coordinador!accessing!private! !
coordinador:!accessing!private! !
costo!accessing!private! !
costo:!accessing!private! !
descripcion!accessing!private! !
descripcion:!accessing!private! !
fecha!accessing!private! !
fecha:!accessing!private! !
id!accessing!private! !
id:!accessing!private! !
inicializarColeccion!public! !
!

!ReunionesGrupales class methodsFor!

incrementarCodReunion
	UltimoCodigo := UltimoCodigo + 1!

inicializarCodigo
	UltimoCodigo:=0.! !
!ReunionesGrupales class categoriesForMethods!
incrementarCodReunion!public! !
inicializarCodigo!public! !
!

SesionesDeGimnasia guid: (GUID fromString: '{cf2c8f8e-cbd3-4341-92f0-3ef5f161e5a0}')!
SesionesDeGimnasia comment: ''!
!SesionesDeGimnasia categoriesForClass!Kernel-Objects! !
!SesionesDeGimnasia methodsFor!

cargaDatos
	self inicializarColeccion.
	MessageBox notify: 'A continuacion ingrese los datos solicitados. ' asString.
	descripcion := (Prompter prompt: 'Ingrese la descripción: ' caption: 'Ingresando datos de la sesion de gimnasia...') asString.
	SesionesDeGimnasia incrementarCodReunion.
	id := UltimoCodigo printString.
	fecha := Date fromString: (Prompter prompt: 'Ingrese la fecha de la reunion DD/MM/YYYY' caption: 'Ingresando datos de la sesion de gimnasia...')!

descripcion
	^descripcion!

descripcion: anObject
	descripcion := anObject!

fecha
	^fecha!

fecha: anObject
	fecha := anObject!

id
	^id!

id: anObject
	id := anObject!

inicializarColeccion
	pacientes := OrderedCollection new! !
!SesionesDeGimnasia categoriesForMethods!
cargaDatos!public! !
descripcion!accessing!private! !
descripcion:!accessing!private! !
fecha!public! !
fecha:!public! !
id!public! !
id:!public! !
inicializarColeccion!public! !
!

!SesionesDeGimnasia class methodsFor!

costo
	^Costo.!

costo: unValor
	Costo:=unValor.!

incrementarCodReunion
	UltimoCodigo := UltimoCodigo + 1!

inicializarCodigo
	UltimoCodigo:=0.! !
!SesionesDeGimnasia class categoriesForMethods!
costo!public! !
costo:!public! !
incrementarCodReunion!public! !
inicializarCodigo!public! !
!

Paciente guid: (GUID fromString: '{a0f9fd7e-9ad7-4142-ba59-92a98c617a1c}')!
Paciente comment: ''!
!Paciente categoriesForClass!Kernel-Objects! !
!Paciente methodsFor!

cargaDatos
	MessageBox notify: 'A continuacion ingrese los datos solicitados. ' caption: 'Cargue datos de paciente.'.
	tipoDni := (Prompter prompt: 'Ingrese tipo DNI: ' caption: 'Ingresando datos de paciente...') asString.
	numeroDni := (Prompter prompt: 'Ingrese numero DNI: ' caption: 'Ingresando datos de paciente...') asNumber.
	apellido := (Prompter prompt: 'Ingrese apellido: ' caption: 'Ingresando datos de paciente...') asString.
	nombre := (Prompter prompt: 'Ingrese nombre' caption: 'Ingresando datos de paciente...') asString.
	direccion := (Prompter prompt: 'Ingrese direccion' caption: 'Ingresando datos de paciente...') asString.
	telefono := (Prompter prompt: 'Ingrese numero telefono: ' caption: 'Ingresando datos de paciente...') asNumber.
	
	Paciente incrementarCodPaciente.
	codigoPaciente:= UltimoCodigo.!

codigoPaciente
	^codigoPaciente!

codigoPaciente: anObject
	codigoPaciente := anObject!

obraSocial
	^obraSocial!

obraSocial: anObject
	obraSocial := anObject! !
!Paciente categoriesForMethods!
cargaDatos!public! !
codigoPaciente!accessing!private! !
codigoPaciente:!accessing!private! !
obraSocial!accessing!private! !
obraSocial:!accessing!private! !
!

!Paciente class methodsFor!

incrementarCodPaciente
	UltimoCodigo:= UltimoCodigo + 1.!

inicializarCodigo
	UltimoCodigo:=0.! !
!Paciente class categoriesForMethods!
incrementarCodPaciente!public! !
inicializarCodigo!public! !
!

Profesional guid: (GUID fromString: '{6ad7102d-8892-40f5-9cfa-5392971e32a2}')!
Profesional comment: ''!
!Profesional categoriesForClass!Kernel-Objects! !
!Profesional methodsFor!

cargaDatos
	MessageBox notify: 'A continuacion ingrese los datos solicitados. ' caption: 'Cargue datos de profesional.'.
	tipoDni := (Prompter prompt: 'Ingrese tipo DNI: 'caption: 'Ingresando datos de profesional...') asString.
	numeroDni := (Prompter prompt: 'Ingrese numero DNI: ' caption: 'Ingresando datos de profesional...') asNumber.
	apellido := (Prompter prompt: 'Ingrese apellido: ' caption: 'Ingresando datos de profesional...') asString.
	nombre := (Prompter prompt: 'Ingrese nombre' caption: 'Ingresando datos de profesional...') asString.
	direccion := (Prompter prompt: 'Ingrese direccion' caption: 'Ingresando datos de profesional...') asString.
	telefono := (Prompter prompt: 'Ingrese numero telefono: ' caption: 'Ingresando datos de profesional...') asNumber.
	matricula := (Prompter prompt: 'Ingrese numero de matricula' caption: 'Ingresando datos de profesional...') asString.
	tarifaConsulta := (Prompter prompt: 'Ingrese tarifa de la consulta: ' caption: 'Ingresando datos de profesional...') asNumber.
	tarifaReunion := (Prompter prompt: 'Ingrese tarifa de la reunion:' caption: 'Ingresando datos de profesional...') asNumber.
	tipoProfesion := (Prompter prompt: 'Ingrese tipo de profesional: ' caption: 'Ingresando datos de profesional...') asString!

matricula
	^matricula!

matricula: anObject
	matricula := anObject!

tarifaConsulta
	^tarifaConsulta!

tarifaConsulta: anObject
	tarifaConsulta := anObject!

tarifaReunion
	^tarifaReunion!

tarifaReunion: anObject
	tarifaReunion := anObject!

tipoProfesion
	^tipoProfesion!

tipoProfesion: anObject
	tipoProfesion := anObject! !
!Profesional categoriesForMethods!
cargaDatos!public! !
matricula!accessing!private! !
matricula:!accessing!private! !
tarifaConsulta!accessing!private! !
tarifaConsulta:!accessing!private! !
tarifaReunion!accessing!private! !
tarifaReunion:!accessing!private! !
tipoProfesion!accessing!private! !
tipoProfesion:!accessing!private! !
!

"Binary Globals"!

