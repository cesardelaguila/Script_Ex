 library(sas7bdat)
library(Information)
library(caret)
datos = read.sas7bdat(file.choose())
variablesN = grep("CR", names(datos))
train = datos[T,-variablesN]

train = train[T,-1]
names(train)

matr =model.matrix(~Reclutamiento+Ocupacion+Jefe_Hogar+Prioridad_Negocio_VD+
                     Porque_VD+Tiene_TC+Tiene_Prestamo+Venta_Competencia+email+               
                     telefono_fijo+telefono_movil, data=train)
matr = matr[T,-1]
train=data.frame(matr,rpta=train$rpta)

inf=create_infotables(train,y="rpta")

fit1 = glm(rpta~Reclutamiento+Jefe_Hogar+Porque_VD+telefono_fijo+
             Tiene_Prestamo,family = binomial(link = "cloglog"), 
           data=train)
fit2 = glm(rpta~Reclutamiento+Jefe_Hogar+telefono_fijo+
                    Tiene_Prestamo,family = binomial(link = "cloglog"), 
                  data=train)
summary(fit1)
summary(fit2)
