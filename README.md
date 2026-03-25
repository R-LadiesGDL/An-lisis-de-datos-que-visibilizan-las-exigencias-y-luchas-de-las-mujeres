Este repositorio analiza la información disponible sobre la violencia de género contra niñas, adolescentes y mujeres en México, utilizando dos de las principales fuentes de datos públicas del país:

Encuesta Nacional sobre la Dinámica de las Relaciones en los Hogares (ENDIREH)
Base de víctimas del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)

A partir de estas fuentes, se desarrolla un análisis exploratorio que permite comprender la magnitud del problema, así como reflexionar sobre los alcances y limitaciones de los datos disponibles.

Fuentes de datos necesarias

Para reproducir este análisis, es necesario contar con los siguientes archivos:

1. ENDIREH 2021
Archivo: bd_endireh_2021.RData
Contiene: base de datos oficial con información sobre violencia contra las mujeres
Debe incluir el objeto: TB_SEC_IVaVD

Disponible en:
https://www.inegi.org.mx/programas/endireh/2021/

2.  SESNSP – Víctimas
Archivo: Base estatal de víctimas (formato .csv)
Ejemplo: Estatal-Víctimas-2015-2025_feb2026.csv
Contiene:
Víctimas por tipo de delito
Sexo
Entidad
Periodo mensual

Disponible en:
https://www.gob.mx/sesnsp/acciones-y-programas/datos-abiertos-de-incidencia-delictiva

Requisitos

Este proyecto está desarrollado en R, por lo que se recomienda:

install.packages(c("tidyverse", "survey"))
