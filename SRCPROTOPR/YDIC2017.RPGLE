000100170118       // --------------------------------------------------
000101170118       // Prototype for procedure: Intento_Init
000102170118       // --------------------------------------------------
000103170118
000104170118       DCL-PR Intento_Init INT(10);
000105170118         piKpjba LIKE(kpjba) CONST OPTIONS(*NOPASS);
000106170118       END-PR ;
000107170118
000108170118       // --------------------------------------------------
000109170118       // Prototype for procedure: Intento_Finalize
000110170118       // --------------------------------------------------
000111170118
000112170118       DCL-PR Intento_Finalize INT(10);
000113170118       END-PR ;
000114170118
000115170119       // --------------------------------------------------
000116170120       // Prototype for procedure: Intento_NewFattura
000117170119       // --------------------------------------------------
000118170119
000119170120       DCL-PR Intento_NewFattura INT(10);
000120170119         piSocieta CHAR(3) CONST;
000121170120         piCliente CHAR(8) CONST;
000123170119         piDataFattura DATE(*ISO) CONST;
000124170120         piImponibileFattura PACKED(15:2) CONST;
000125170123         piClientePartitaIVA CHAR(20) CONST OPTIONS(*NOPASS);
000126170119       END-PR ;
000127170119
000128170120       // --------------------------------------------------
000129170120       // Prototype for procedure: Intento_EndFattura
000130170120       // --------------------------------------------------
000131170120
000132170120       DCL-PR Intento_EndFattura INT(10);
000133170120       END-PR ;
000134170120
000135170120       // --------------------------------------------------
000136170120       // Prototype for procedure: Intento_GetImponibileFatturatoSenzaIvaAnno
000137170120       // --------------------------------------------------
000138170120
000139170120       DCL-PR Intento_GetImponibileFatturatoSenzaIvaAnno PACKED(15:2);
000140170120       END-PR ;
000141170120
000142170120       // --------------------------------------------------
000143170123       // Prototype for procedure: Intento_GetUltimaDichiarazione
000144170120       // --------------------------------------------------
000145170120
000146170123       DCL-PR Intento_GetUltimaDichiarazione INT(10);
000147170120         piDataCliente DATE(*ISO);
000148170120         piNumeroCliente INT(10);
000149170120         piDataInterna DATE(*ISO);
000150170120         piNumeroInterno INT(10);
000151170120         piDataInizio DATE(*ISO);
000152170120         piDataFine DATE(*ISO);
000153170120         piImportoDichiarazione PACKED(15:2);
000154170120         piImportoDichiarazioniAnno PACKED(15:2);
000155170120         piCodiceIVA CHAR(3);
000156170120         piAgenziaEntrateProtocollo CHAR(17);
000157170120         piAgenziaEntrateProgressivo CHAR(6);
000158170120       END-PR ;
000159170120
000160170123       // --------------------------------------------------
000161170123       // Prototype for procedure: Intento_GetFraseFattura
000162170123       // --------------------------------------------------
000163170123
000164170123       DCL-PR Intento_GetFraseFattura VARCHAR(255);
000165170123       END-PR ;
000166170123
000167170125       // --------------------------------------------------
000168170125       // Prototype for procedure: Intento_GetImportoDichiarazioniAnno
000169170125       // --------------------------------------------------
000170170125
000171170125       DCL-PR Intento_GetImportoDichiarazioniAnno PACKED(15:2);
000172170125       END-PR ;
000173170125
000174170125       // --------------------------------------------------
000175170125       // Prototype for procedure: Intento_GetCodiceIVA
000176170125       // --------------------------------------------------
000177170125
000178170125       DCL-PR Intento_GetCodiceIVA CHAR(3);
000179170125       END-PR ;
000180170125
