##########################################
######## Paolo Valcarcel Pineda ##########
######### Camila Cirulo Aragon ###########
######### Proto modelo de Gasto ##########
##########################################

clear all
cd "D:\NUEVO D\GEIH 2021\ENCV"

*Dependiente
// Gasto en educación

*Independientes
//Máximo logro educativo del jefe de hogar
//Estado civil del jefe de hogar
//Sexo del jef de hogar
//Si el hogar tiene electricidad
//Número de hijos y nietos en edad escolar
//Hogar rural o urbano
//Ocupados
//Perceptores de ingreso
//Ingreso por hogar


use Educación, clear
global id "directorio secuencia_encuesta secuencia_p orden"
merge 1:1 $id using "Caracteristicas y composicion del hogar", gen(fus1) //Personas
drop if fus1==2
merge 1:1 $id using "Fuerza de trabajo", gen(fus2)  //Personas
drop if fus2==1
merge 1:1 $id using "Caracteristicas y composicion del hogar", gen(fus3) //Personas
drop if fus3==2
merge 1:1 $id using "Tecnologias de información y comunicación", gen(fus4) //Personas 
drop if fus4==2


forvalues i=1/4 {
tab fus`i'	
}

drop fus1 fus2 fus3 fus4

*p6180s1 //Valor de comidas
*p3341s1 //matrículas
*p3342s1 //uniformes
*p3343s1 //alquiler de útiles
*p3344s1 //pensión
*p3345s1 //transporte escolar
*p3346s1 //alimentos
*p3347s1 //útiles escolares
*p3348s1 //otros gastos educativos
*p8610s1 //becas
*p8614s1 //crédito educativo

egen gas_educ=rowtotal(p6180s1 p3341s1 p3342s1 p3343s1 p3344s1 p3345s1 p3346s1 p3347s1 p3348s1 p8610s1 p8614s1)

/*
collapse (sum) gas_educ, by(directorio)
gen d=1 if gas_educ!=0
replace d=0 if d==.
tab d
save id_dir, replace */

tab p6051 //Relación de parentesco (1 jefe del hogar)

//Educación
*tab p6087 //máximo nivel educativo alcanzado por el padre
*tab p6088 //máximo nivel educativo alcanzado por la madre
*gen educ_padre=p6087 if p6051==1
*gen educ_madre=p6088 if p6051==1
//br directorio secuencia_p orden p6087 p6088 p6051 educ_padre educ_madre

gen educ=p8587 if p6051==1
recode educ (1 2 = 1) (3 4= 2) (5=3) (6 7 = 4) (8 9 =5) (10 11=6) (12 13=7), gen(educ_final)
tab educ_final

lab define educ_final 1 "Ninguno" 2 "Básica primaria" 3"Media" 4 "Técnica" 5 "Tecnológico" 6 "Universitaria" 7 "Posgrado"  

lab val educ_final educ_final

//estado civil
tab p5502 
gen est_civ=p5502 if p6051==1

//sexo (no va)
tab p6020   
gen sexo=p6020 if p6051==1

//hijos
tab p6051 
egen hijos=count(p6051) if p6051==3, by(directorio orden)
egen hijos2=sum(hijos), by(directorio)
gen nhijos=hijos2 if p6051==1
drop hijos hijos2

//br directorio secuencia_p orden p6051 nhijos

//ocupados
//tab p6240
//gen ocu=1 if p6240==1

collapse (sum)gas_educ (max)educ_final (max)est_civ (max)nhijos, by(directorio)
merge 1:m directorio using "Servicios del hogar", gen(fus5) //Hogar


tab p791 //si tiene electricidad
*br p5018 //pago por electricidad
gen electricidad=1 if p791==1 | p792==2
replace electricidad=0 if electricidad==.

merge 1:m $id using "Datos de la vivienda", gen(fus6) //Hogar (88,722 obs)
drop if fus6==1 | fus6==2

gen urbano=1 if clase==1
replace urbano=0 if clase==2

gen ingreso=i_hogar
replace educ_final=1 if educ_final==.
mdesc ingreso urbano gas_educ educ_final est_civ nhijos //Para revisar los missings

count if gas_educ==0
drop if gas_educ==0 |  gas_educ==99
count if ingreso==0
drop if ingreso==0

keep directorio secuencia_p orden fex_c gas_educ ingreso urbano educ_final est_civ nhijos electricidad
reg  gas_educ ingreso urbano i.educ_final i.est_civ nhijos electricidad 
gen log_educ=ln(gas_educ)
reg  log_educ ingreso urbano i.educ_final i.est_civ nhijos electricidad 
