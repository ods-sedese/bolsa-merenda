# Bolsa Merenda

Este é o repositório do Programa Bolsa Merenda. Nele temos o código e os arquivos utilizados para a estruturação do Programa com exceção dos dados sigilosos. Caso alguém tenha interesse em reproduzir o dashboard localmente é só digitar o seguinte código no R:

```
if (!require('shiny')) install.packages("shiny")
shiny::runGitHub("bolsa-merenda", "ods-sedese")
```

## O que é o Programa Bolsa Merenda

O Bolsa Merenda é um Benefício temporário destinado à prestação de assistência alimentar às famílias de estudantes matriculados da Rede Pública Estadual de ensino no valor de R$ 50,00 (cinquenta reais), por aluno, às famílias em condições de pobreza e extrema pobreza, para compra de alimentos e possui a finalidade de reduzir os efeitos socioeconômicos decorrentes das ações de enfrentamento da pandemia de Covid-19.

O benefício iniciou em abril de 2020 e foram concedidas 4 parcelas aos extremamente pobres. Em setembro foram concedidas mais duas parcelas aos extremamente pobres e também foram concedidas duas parcelas às famílias em condições de pobreza, conforme Decreto Federal nº 9.396, de 30 de maio de 2018.

Para mais informações, acesse https://bolsamerenda.social.mg.gov.br/

## Objetivo

O benefício temporário, exclusivo para alunos da rede estadual de ensino inscritos no CadÚnico, tem o objetivo de reduzir os impactos da suspensão das aulas em função da pandemia do novo coronavírus e garantir a segurança alimentar de aproximadamente 470 mil crianças e adolescentes.

## Estruturação do repositório

Os arquivos 'app.R', 'data', 'helpers' e 'www' são utilizados para a reprodução do dashboard. 

<!---
A pasta Notas Técnicas tem os estudos realizados para a estruturação do Programa.
-->