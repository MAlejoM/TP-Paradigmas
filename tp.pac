| package |
package := Package name: 'TP_Paradigmas_Grupo_6'.
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
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

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
	instanceVariableNames: 'descripcion costo coordinador pacientes id fecha'
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
	msj := 'Listado de Reuniones:' , String cr.
	profe := ''.
	minutaPorRegistrar := MinutaActividad new.
	reunionesGrupales do: 
			[:reunion |
			Date today <= reunion fecha
				ifTrue: 
					[profe := self busquedaProfesionalDni: reunion coordinador numeroDni.
					msj := msj , '. ID: ' , reunion id , ' - Fecha: ' , reunion fecha printString , String cr
								, '            - Coordinador: ' , profe nombre printString
								, ' ' , profe apellido printString
								, String cr]].
	MessageBox notify: msj , String cr , 'A continuacion se solicitara el ID de Reunion...'
		caption: 'Asignando Paciente a Reunion.'.
	eleccion := Prompter prompt: 'ID de Reunion:' caption: 'Asignando Paciente a Reunion.'.
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
							MessageBox notify: 'Se agrego Paciente correctamente' caption: 'Aviso.'.
							minutaPorRegistrar actividad: reunion.
							minutaPorRegistrar paciente: pac.
							MinutaActividad incrementarCod.
							minutaPorRegistrar fechaAsistencia: reunion fecha.
							minutaPorRegistrar pesoPaciente: 0.
							minutaPorRegistrar costo: costoMinuta.
							minutaPorRegistrar id: MinutaActividad ultimoCodigo.
							minutasDeActividad add: minutaPorRegistrar]]]!

agregarPacienteSesionG
	| msj eleccion pac minutaPorRegistrar costoMinuta |
	costoMinuta := SesionesDeGimnasia costo.
	eleccion := ''.
	msj := 'Listado de Sesiones:' , String cr.
	minutaPorRegistrar := MinutaActividad new.
	sesionesGimnasias do: 
			[:sesion |
			Date today <= sesion fecha
				ifTrue: [msj := msj , '. ID: ' , sesion id , ' - Fecha: ' , sesion fecha printString , String cr]].
	MessageBox notify: msj , String cr , 'A continuacion se solicitara el ID de la Sesion...' caption: ' Asignando Paciente a Sesion.'.
	eleccion := Prompter prompt: 'ID de Sesion:' caption: ' Asignando Paciente a Sesion.'.
	msj := ''.
	sesionesGimnasias do: 
			[:sesion |
			sesion id = eleccion
				ifTrue: 
					[pac := self muestreoPaciente.
					pac = 0
						ifFalse: 
							[sesion agregarPaciente: pac.
							MessageBox notify: 'Se agrego Paciente correctamente' caption: 'Aviso.'.
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
	msj := 'Listado:' , String cr.
	obrasSociales do: 
			[:obra |
			msj := msj , '. ', obra nombre printString , '   Codigo: ' , obra codigo printString
						, String cr].
	MessageBox notify: msj , String cr , 'A continuacion se solicitara el codigo...' caption: 'Asignando Obra Social.'.
	codigo := (Prompter
				prompt: 'Si tiene obra social ingrese el codigo de la misma, de lo contrario deje el campo vacio: '
					caption: 'Asignando Obra Social') asNumber.
	credencial := obrasSociales detect: [:each | each codigo = codigo 	] ifNone: [nil].
	credencial isNil
		ifTrue: 
			[codigo = 0
				ifTrue: 
					[MessageBox notify:'Paciente asignado correctamente.'caption: 'Asignando Obra Social' .
					^0]
				ifFalse: [MessageBox warning: 'CODIGO INEXISTENTE' caption: 'Error.']]
		ifFalse: 
			[MessageBox notify: 'Paciente asignado correctamente.' caption:'Asignando Obra Social'.
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
			[fecha1 := Date fromString: (Prompter prompt: 'Fecha de Inicio del rango (DD/MM/YYYY):' caption: 'Informe para Obra Social.').
			fecha2 := Date fromString: (Prompter prompt: 'Fecha de Fin del rango (DD/MM/YYYY):' caption: 'Informe para Obra Social.').
			minutasDeActividad do: 
					[:min |
					min pesoPaciente = 0
						ifFalse: 
							[min fechaAsistencia <= fecha2 & (min fechaAsistencia >= fecha1) & (min paciente obraSocial = os)
								ifTrue: 
									[msj := msj ,'. ', min paciente nombre , ' ' , min paciente apellido, ' - Costo: $' , min costo printString , String cr.
									total := total + min costo]]].
			msj := msj , String cr , 'El costo total es de: $' , total printString , String cr
						, 'La obra social debe cubrir: $' , (total * os porcentajeServicio / 100) printString.
			MessageBox notify: msj caption: 'Informe para Obra Social.']!

informeReunion
	| msj msj2 eleccion costoTotal |
	eleccion := ''.
	msj := ''.
	reunionesGrupales do: 
			[:reunion |
			msj := msj , '.  ID: ' , reunion id , ' - Fecha: ' , reunion fecha printString , String cr
						, '            - Coordinador: ' , reunion coordinador nombre
						, '  ' , reunion coordinador apellido
						, String cr].
	MessageBox notify: msj , String cr , 'A continuacion se solicitara el ID de Reunion...'
		caption: 'Seleccion de Reunion.'.
	eleccion := Prompter prompt: 'ID de la Reunion:' caption: 'Informe de Reunion'.
	msj := 'Listado de pacientes:' , String cr.
	reunionesGrupales do: 
			[:reunion |
			reunion id = eleccion
				ifTrue: 
					[reunion pacientes do: 
							[:pacien |
							msj := msj , '. ' , pacien nombre , ' ' , pacien apellido , '  DNI: ' , pacien numeroDni printString
										, String cr].
					costoTotal := reunion costo + reunion coordinador tarifaReunion.
					msj2 := 'Coordinador: ' , reunion coordinador nombre , ' ' , reunion coordinador apellido
								, String cr , 'Costo total: $'
								, costoTotal printString , String cr
								, 'Descripcion: ' , reunion descripcion
								, String cr , 'Fecha: '
								, reunion fecha printString , String cr
								, msj , String cr.
					MessageBox notify: msj2 caption: 'Informe de Reunion.']]!

informeSesion
	| msj msj2 eleccion  |
	eleccion := ''.
	msj := ''.
	sesionesGimnasias do: [:sesion | msj := msj , '. ID: ' , sesion id , ' - Fecha: ' , sesion fecha printString , String cr].
	MessageBox notify: msj , String cr , 'A continuacion se solicitara el ID de Sesion...' caption: 'Seleccion de Sesion.'.
	eleccion := Prompter prompt: 'ID de la Sesion:' caption: 'Informe de Sesion'.
	msj := 'Listado de pacientes:' , String cr.
	sesionesGimnasias do: 
			[:sesion |
			sesion id = eleccion
				ifTrue: 
					[sesion pacientes do: 
							[:pacien |
							msj := msj , '. ' , pacien nombre , ' ' , pacien apellido , '  DNI: '
										, pacien numeroDni printString , String cr].
					
					msj2 := 'Costo total: $' , SesionesDeGimnasia costo printString , String cr , 'Descripcion: ' , sesion descripcion
								, String cr ,' Fecha: ', sesion fecha printString
								, String cr , msj
								, String cr.
					MessageBox notify: msj2 caption: 'Informe de Sesion.']]!

inicializar
	| x |
	x := (Prompter prompt: 'Ingrese el costo para las Sesiones de Gimnasia: '
				caption: 'Costo de Sesiones de Gimnasia.')asNumber.
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
01- Alta Obra Social.
02- Alta Profesional.
03- Alta Paciente.
04- Alta Reunion.
05- Alta Sesion.
06- Alta Consulta.
07- Agregar Paciente a Reunion.
08- Agregar Paciente a Sesion de Gimnasia.
09- Registrar Minuta de Actividad.
10- Generar Deuda de Obra Social.
11- Generar Informe de Reuniones.
12- Generar Informe de Sesiones de Gimnasia.
00- Salir.

Ingrese la opcion a continuacion...
'.
			op := (Prompter prompt: 'Ingrese opción:') asNumber asInteger.
			op = 1 ifTrue: [self regObraSocial].
			op = 2 ifTrue: [self regProfesional].
			op = 3
				ifTrue: 
					[cantidad := obrasSociales size.
					cantidad = 0
						ifTrue: 
							[MessageBox
								warning: 'Para registrar un paciente se necesita al menos una Obra Social cargada, para continuar registre al menos una. '
								caption: 'Error.']
						ifFalse: [self regPaciente]].
			op = 4
				ifTrue: 
					[cantidad := profesionales size.
					cantidad = 0
						ifTrue: 
							[MessageBox
								warning: 'Para registrar una Reunion se necesita al menos un Profesional registrado, para continuar registre al menos uno. '
								caption: 'Error.']
						ifFalse: [self regReunion]].
			op = 5 ifTrue: [self regSesionGimnasia].
			op = 6
				ifTrue: 
					[cantidad := profesionales size.
					cantidad = 0
						ifTrue: 
							[MessageBox
								warning: 'Para registrar una Consulta se necesita al menos un Profesional registrado, para continuar registre al menos uno. '
								caption: 'Error.']
						ifFalse: 
							[cantidad := pacientes size.
							cantidad = 0
								ifTrue: [MessageBox warning: 'No hay Pacientes registrados, para continuar registre al menos uno. ']
								ifFalse: [self regConsultasClinicas]]].
			op = 7
				ifTrue: 
					[cantidad := pacientes size.
					cantidad = 0
						ifTrue: 
							[MessageBox warning: 'No hay Pacientes registrados, para continuar registre al menos uno. '
								caption: 'Error.']
						ifFalse: 
							[cantidad := reunionesGrupales size.
							cantidad = 0
								ifTrue: 
									[MessageBox
										warning: 'Para registrar un paciente se necesita al menos una Reunion cargada, para continuar registre al menos una. '
										caption: 'Error.']
								ifFalse: [self agregarPacienteReunion]]].
			op = 8
				ifTrue: 
					[cantidad := pacientes size.
					cantidad = 0
						ifTrue: 
							[MessageBox warning: 'No hay Pacientes registrados, para continuar ingrese al menos uno. '
								caption: 'Error']
						ifFalse: 
							[cantidad := sesionesGimnasias size.
							cantidad = 0
								ifTrue: 
									[MessageBox
										warning: 'Para registrar un paciente se necesita al menos una Sesion de Gimnasia cargada, para continuar registre al menos una. '
										caption: 'Error.']
								ifFalse: [self agregarPacienteSesionG]]].
			op = 9
				ifTrue: 
					[cantidad := minutasDeActividad size.
					cantidad = 0
						ifTrue: 
							[MessageBox
								warning: 'No hay Actividades registradas/asignadas, para continuar registre al menos una. '
								caption: 'Error.']
						ifFalse: [self regMinutaActividad]].
			op = 10
				ifTrue: 
					[cantidad := obrasSociales size.
					cantidad = 0
						ifTrue: 
							[MessageBox warning: 'No hay Obras Sociales registradas, para continuar ingrese al menos una. '
								caption: 'Error']
						ifFalse: [self informeParaObraSocial]].
			op = 11
				ifTrue: 
					[cantidad := reunionesGrupales size.
					cantidad = 0
						ifTrue: 
							[MessageBox warning: 'No hay Reuniones registradas, para continuar ingrese al menos una. '
								caption: 'Error']
						ifFalse: [self informeReunion]].
			op = 12
				ifTrue: 
					[cantidad := sesionesGimnasias size.
					cantidad = 0
						ifTrue: 
							[MessageBox warning: 'No hay Sesiones de Gimnasia registradas, para continuar ingrese al menos una. '
								caption: 'Error']
						ifFalse: [self informeSesion]]]!

muestreoObraSocial
	| msj credencial codigo |
	codigo := ''.
	msj := 'Listado:' , String cr.
	obrasSociales do: 
			[:obra |
			msj := msj , '. ', obra nombre printString , '   Codigo: ' , obra codigo printString
						, String cr].
	MessageBox notify: msj , String cr , 'A continuacion se solicitara el codigo...' caption: 'Obras Sociales Disponibles'.
	codigo := (Prompter
				prompt: 'Codigo:'
				caption: 'Seleccion de Obra Social.') asNumber.
	credencial := obrasSociales detect: [:each | each codigo = codigo] ifNone: [nil].
	credencial isNil
		ifTrue: 
			[MessageBox warning: 'CODIGO INEXISTENTE.' caption:'Error.'.
			^0]
		ifFalse: 
			[^credencial]!

muestreoPaciente
	| msj dniSeleccionado pac |
	msj := 'Listado:' , String cr.
	pacientes
		do: [:pacien | msj := msj , '. ' , pacien nombre ,' ', pacien apellido, '  DNI: ' , pacien numeroDni printString , String cr].
	MessageBox notify: msj , String cr , 'A continuacion seleccione el DNI del Paciente...'
		caption: 'Pacientes registrados.'.
	dniSeleccionado := (Prompter prompt: 'Seleccione DNI de Paciente: ' caption: 'Seleccion de Paciente.') asNumber.
	pac := pacientes detect: [:each | each numeroDni = dniSeleccionado] ifNone: [nil].
	pac isNil
		ifTrue: 
			[MessageBox warning: 'NO EXISTE EL DNI' caption: 'Error.'.
			^0]
		ifFalse: 
			[MessageBox notify: 'Se seleccionó el Paciente correctamente.' caption:'Aviso.'. 
			^pac]!

muestreoProfesional
	| msj dniSeleccionado pro |
	msj := 'Listado: ' , String cr.
	profesionales do: 
			[:profe |
			msj := msj , '. ', profe nombre , ' ' , profe apellido , '  DNI: ' , profe numeroDni printString
						,'  - ', profe tipoProfesion , String cr].
	MessageBox notify: msj , String cr , 'A continuacion seleccione el DNI del Profesional...'
caption: 'Profesionales registrados.'.
	dniSeleccionado := (Prompter prompt: 'Seleccione DNI de Profesional: ' caption: 'Seleccion de Profesional.') asNumber.
	pro := profesionales detect: [:each | each numeroDni = dniSeleccionado] ifNone: [nil].
	pro isNil
		ifTrue: 
			[MessageBox warning: 'NO EXISTE EL DNI'.
			^0]
		ifFalse: 
			[MessageBox notify: 'Se seleccionó el Profesional correctamente.' caption:'Aviso.'. 
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
	MessageBox notify: 'A continuacion ingrese los datos solicitados...' caption: 'Registro de Minuta de Actividad.'.
	pac := self muestreoPaciente.
	pac = 0
		ifFalse: 
			[minutasDeActividad do: 
					[:minun |
					minun actividad fecha = Date today & (minun paciente = pac) & (minun pesoPaciente = 0)
						ifTrue: 
							[msj := msj , 'ID:  ' , minun id printString , '  ' , minun paciente nombre ,'  ', minun paciente apellido, '  DNI: '
										, minun paciente numeroDni printString , ' - '
										, minun actividad class printString , String cr]].
			MessageBox notify: msj , String cr , 'A continuacion seleccione el ID de la Actividad...' caption:'Registro de Minuta de Actividad.'.
			idSeleccionado := (Prompter prompt: 'ID de Actividad:' caption:'Registro de Minuta de Actividad.') asNumber.
	minutaPorIngresar := minutasDeActividad detect: [:each | each id = idSeleccionado] ifNone: [nil].
	minutaPorIngresar pesoPaciente: (Prompter prompt: 'Ingrese el Peso del Paciente (KG):' caption:'Registro de Minuta de Actividad.') asNumber.
	profe := MessageBox confirm: 'Desea ingresar un profesional de asistencia?' caption:'Registro de Minuta de Actividad.'.
	profe = true
		ifTrue: 
			[profe := self muestreoProfesional.
			profe = 0
				ifFalse: 
					[minutaPorIngresar profesional: profe.
					consultasClinicas add: minutaPorIngresar.
					MessageBox notify: 'Se registro la Minuta correctamente.' caption: 'Registro de Minuta de Actividad.']]
		ifFalse: 
			[minutaPorIngresar profesional: 0.
			consultasClinicas add: minutaPorIngresar.
			MessageBox notify: 'Se registro la Minuta correctamente.' caption: 'Registro de Minuta de Actividad.']].!

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
	MessageBox notify: 'A continuacion ingrese los datos solicitados. ' caption: 'Alta de Consultas Clinicas.'.
	descripcion := (Prompter prompt: 'Descripcion de la Consulta:'
				caption: 'Alta de Consultas Clinicas.') asString.
	tipoConsulta := (Prompter prompt: 'Tipo de consulta:'
				caption: 'Alta de Consultas Clinicas.') asString.
	fecha := Date fromString: (Prompter prompt: 'Fecha de realizacion (DD/MM/YYYY):'
						caption: 'Alta de Consultas Clinicas.')!

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
	MessageBox notify: 'A continuacion ingrese los datos solicitados. ' caption: 'Alta de Obra Social.'.
	nombre := (Prompter prompt: 'Nombre: ' caption: 'Alta de Obra Social.') asString.
	codigo := (Prompter prompt: 'Codigo: ' caption: 'Alta de Obra Social.') asNumber.
	porcentajeServicio := (Prompter prompt: 'Porcentaje de descuento: ' caption: 'Alta de Obra Social.')
				asNumber!

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
	pacientes add: unPaciente!

cargaDatos
	self inicializarColeccion.
	MessageBox notify: 'A continuacion ingrese los datos solicitados. ' caption: 'Alta de Reunion Grupal.' asString.
	descripcion := (Prompter prompt: 'Descripcion de la reunion: ' caption: 'Alta de Reunion Grupal.') asString.
	costo := (Prompter prompt: 'Costo: ' caption: 'Alta de Reunion Grupal.') asNumber.
	ReunionesGrupales incrementarCodReunion.
	id := UltimoCodigo printString.
	fecha := Date fromString: (Prompter prompt: 'Fecha de realizacion (DD/MM/YYYY):' caption: 'Alta de Reunion Grupal.')!

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
	pacientes := OrderedCollection new!

pacientes
	^pacientes!

pacientes: anObject
	pacientes := anObject! !
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
pacientes!accessing!private! !
pacientes:!accessing!private! !
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

agregarPaciente: unPaciente
	pacientes add: unPaciente!

cargaDatos
	self inicializarColeccion.
	MessageBox notify: 'A continuacion ingrese los datos solicitados. ' caption: 'Alta de Sesion de Gimnasia.'asString.
	descripcion := (Prompter prompt: 'Descripción  de la Sesion:' caption: 'Alta de Sesion de Gimnasia.') asString.
	SesionesDeGimnasia incrementarCodReunion.
	id := UltimoCodigo printString.
	fecha := Date fromString: (Prompter prompt: 'Fecha de realizacion (DD/MM/YYYY):' caption: 'Alta de Sesion de Gimnasia.')!

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
	pacientes := OrderedCollection new!

pacientes
	^pacientes!

pacientes: anObject
	pacientes := anObject! !
!SesionesDeGimnasia categoriesForMethods!
agregarPaciente:!public! !
cargaDatos!public! !
descripcion!accessing!private! !
descripcion:!accessing!private! !
fecha!public! !
fecha:!public! !
id!public! !
id:!public! !
inicializarColeccion!public! !
pacientes!accessing!private! !
pacientes:!accessing!private! !
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
	MessageBox notify: 'A continuacion ingrese los datos solicitados. ' caption: 'Alta de Paciente.'.
	tipoDni := (Prompter prompt: 'Tipo de Documento: ' caption: 'Alta de Paciente.') asString.
	numeroDni := (Prompter prompt: 'Numero de Documento: ' caption: 'Alta de Paciente.') asNumber.
	nombre := (Prompter prompt: 'Nombre: ' caption: 'Alta de Paciente.') asString.
	apellido := (Prompter prompt: 'Apellido: ' caption: 'Alta de Paciente.') asString.
	direccion := (Prompter prompt: 'Direccion:' caption: 'Alta de Paciente.') asString.
	telefono := (Prompter prompt: 'Numero de Telefono:' caption: 'Alta de Paciente.') asNumber.
	Paciente incrementarCodPaciente.
	codigoPaciente := UltimoCodigo!

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
	MessageBox notify: 'A continuacion ingrese los datos solicitados. ' caption: 'Alta de Profesional.'.
	tipoDni := (Prompter prompt: 'Tipo de Documento:' caption: 'Alta de Profesional.') asString.
	numeroDni := (Prompter prompt: 'Numero de Documento: ' caption: 'Alta de Profesional.') asNumber.
	nombre := (Prompter prompt: 'Nombre:' caption: 'Alta de Profesional.') asString.
	apellido := (Prompter prompt: 'Apellido:' caption: 'Alta de Profesional.') asString.
	direccion := (Prompter prompt: 'Direccion:' caption: 'Alta de Profesional.') asString.
	telefono := (Prompter prompt: 'Numero de Telefono: ' caption: 'Alta de Profesional.') asNumber.
	matricula := (Prompter prompt: 'Numero de Matricula:' caption: 'Alta de Profesional.') asString.
	tarifaConsulta := (Prompter prompt: 'Tarifa de Consulta:' caption: 'Alta de Profesional.') asNumber.
	tarifaReunion := (Prompter prompt: 'Tarifa de Reunion Grupal:' caption: 'Alta de Profesional.') asNumber.
	tipoProfesion := (Prompter prompt: 'Tipo de Profesional:' caption: 'Alta de Profesional.') asString!

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

