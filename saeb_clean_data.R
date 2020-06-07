library(data.table)

student <- fread('G:/dataset_novo/microdados_saeb_2017/DADOS/TS_ALUNO_9EF.csv')
head(student)

student <- student[, c('ID_MUNICIPIO', 'ID_ESCOLA', 'PROFICIENCIA_LP_SAEB',
				   'PROFICIENCIA_MT_SAEB')]

head(student)

school <- fread('G:/dataset_novo/microdados_saeb_2017/DADOS/TS_ESCOLA.csv')
head(school)
school <- school[, c('ID_MUNICIPIO', 'ID_ESCOLA', 'PC_FORMACAO_DOCENTE_INICIAL',
			   'PC_FORMACAO_DOCENTE_FINAL', 'NU_MATRICULADOS_CENSO_9EF', 
			   'NU_PRESENTES_9EF', 'MEDIA_9EF_LP', 'MEDIA_9EF_MT')]

student <- subset(student, select = -c(ID_ESCOLA))

###Deletando os municípios que nao fizeram a prova 
student <- student[!is.na(student$PROFICIENCIA_MT_SAEB), ]

school <- subset(school, select = -c(ID_ESCOLA))
school <- school[!is.na(school$MEDIA_9EF_MT), ]

census_school <- fread('G:/dataset_novo/Microdados_Censo_Escolar_2017/DADOS/ESCOLAS.CSV')
census_school <- census_school[, c('CO_MUNICIPIO', 'NU_SALAS_EXISTENTES', 
 			               'NU_SALAS_UTILIZADAS', 'NU_EQUIP_TV',
					   'NU_EQUIP_DVD', 'NU_EQUIP_PARABOLICA', 
					   'NU_EQUIP_COPIADORA', 'NU_EQUIP_RETROPROJETOR',
					   'NU_EQUIP_IMPRESSORA', 'NU_EQUIP_IMPRESSORA_MULT',
				         'NU_EQUIP_SOM', 'NU_EQUIP_MULTIMIDIA',
					   'NU_COMPUTADOR', 'NU_COMP_ADMINISTRATIVO',
					   'NU_COMP_ALUNO', 'NU_FUNCIONARIOS')]

census_school <- census_school[order(census_school$CO_MUNICIPIO), ]

head(census_school)

#Agregation
## Builting the center of the polygons
center_student <- aggregate(student, by = list(c(student$ID_MUNICIPIO)), 
				  FUN = mean, na.rm = TRUE)

center_school <- aggregate(school, by = list(c(school$ID_MUNICIPIO)), 
				   FUN = mean, na.rm = TRUE)

center_census <- aggregate(census_school, by = list(c(census_school$CO_MUNICIPIO)), 
				  FUN = mean, na.rm = TRUE)

center_student$Group.1 <- NULL
center_school$Group.1 <- NULL
center_census$Group.1 <- NULL


## Builting the radius of the polygons
radius_student <- aggregate(student, by = list(c(student$ID_MUNICIPIO)), 
				  FUN = function(x) 2 * sd(x, na.rm = TRUE))

radius_school <- aggregate(school, by = list(c(school$ID_MUNICIPIO)), 
				   FUN = function(x) 2 * sd(x, na.rm = TRUE))

radius_census <- aggregate(census_school, by = list(c(census_school$CO_MUNICIPIO)), 
				  FUN = function(x) 2 * sd(x, na.rm = TRUE))

radius_student$ID_MUNICIPIO <- NULL
radius_school$ID_MUNICIPIO <- NULL
radius_census$CO_MUNICIPIO <- NULL

names(radius_student)[names(radius_student) == 'Group.1'] <- 'ID_MUNICIPIO'
names(radius_school)[names(radius_school) == 'Group.1'] <- 'ID_MUNICIPIO'
names(radius_census)[names(radius_census) == 'Group.1'] <- 'ID_MUNICIPIO'

#Merge the datasets of the center and radius
center <- merge(center_student, center_school, by.x = 'ID_MUNICIPIO', 
		    by.y = 'ID_MUNICIPIO')
center <- merge(center, center_census, by.x = 'ID_MUNICIPIO', 
		    by.y = 'CO_MUNICIPIO')
center1 <- center[complete.cases(center), ]

radius <- merge(radius_student, radius_school, by.x = 'ID_MUNICIPIO', 
		    by.y = 'ID_MUNICIPIO')
radius <- merge(radius, radius_census, by.x = 'ID_MUNICIPIO', 
		    by.y = 'ID_MUNICIPIO')
radius1 <- radius[complete.cases(radius), ]

dat <- merge(center1, radius1, by.x = 'ID_MUNICIPIO', by.y = 'ID_MUNICIPIO')
head(dat)

id_municipio <- dat$ID_MUNICIPIO
p <- ncol(dat[, -1]) / 2

center2 <- dat[, 1 : (p + 1)]
radius2 <- dat[, c(1, (p + 2) : (2 * p + 1))]

names(center2) <- names(center)
names(radius2) <- names(radius)

center2$MEDIA_9EF_LP <- NULL
center2$MEDIA_9EF_MT <- NULL
radius2$MEDIA_9EF_LP <- NULL
radius2$MEDIA_9EF_MT <- NULL

center3 <- center2[, c('ID_MUNICIPIO', 'PROFICIENCIA_LP_SAEB',
				'PROFICIENCIA_MT_SAEB', 'NU_SALAS_EXISTENTES',
				'NU_SALAS_UTILIZADAS', 'NU_COMPUTADOR',
				'NU_FUNCIONARIOS')]

radius3 <- radius2[, c('ID_MUNICIPIO', 'PROFICIENCIA_LP_SAEB',
				'PROFICIENCIA_MT_SAEB', 'NU_SALAS_EXISTENTES',
				'NU_SALAS_UTILIZADAS', 'NU_COMPUTADOR',
				'NU_FUNCIONARIOS')]


setwd('G:/Dropbox_Novo/Dropbox/2. Doutorado/4. Artigos/Software Article/saeb_2017')

write.table(center3, 'saeb2017_center.csv', row.names = F, sep = ',')
write.table(radius3, 'saeb2017_radius.csv', row.names = F, sep = ',')




