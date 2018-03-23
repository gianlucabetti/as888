000100170710       // --------------------------------------------------
000101170710       // Prototype for procedure: YIVASPLIT_Init
000102170710       // --------------------------------------------------
000103170710
000104170710       DCL-PR YIVASPLIT_Init INT(10);
000105170711         prmKpjba LIKEDS(kpjba) CONST OPTIONS(*NOPASS);
000106170710       END-PR ;
000107170710
000108170710       // --------------------------------------------------
000109170710       // Prototype for procedure: YIVASPLIT_Finalize
000110170710       // --------------------------------------------------
000111170710
000112170710       DCL-PR YIVASPLIT_Finalize ;
000113170710       END-PR ;
000114170710
000115170710       // --------------------------------------------------
000116170712       // Prototype for procedure: YIVASPLIT_NewDocumentoFattura
000117170710       // --------------------------------------------------
000118170710
000119170712       DCL-PR YIVASPLIT_NewDocumentoFattura INT(10);
000120170710         prmPartitaIvaCliente CHAR(20) CONST;
000121170711         prmCodiceFiscaleCliente CHAR(20) CONST;
000122170710         prmSocietaDocumento CHAR(3) CONST;
000123170710         prmDataDocumento DATE(*ISO) CONST;
000124170710         prmLibroDocumento CHAR(3) CONST;
000126170710       END-PR ;
000127170712
000128170712       // --------------------------------------------------
000129170712       // Prototype for procedure: YIVASPLIT_NewDocumentoNA
000130170712       // --------------------------------------------------
000131170712
000132170712       DCL-PR YIVASPLIT_NewDocumentoNA INT(10);
000138170712         prmSocietaPartita CHAR(3) CONST;
000139170712         prmKccPartita CHAR(6) CONST;
000140170712         prmClientePartita CHAR(8) CONST;
000141170712         prmDataPartita DATE(*ISO) CONST;
000142170712         prmNumeroPartita PACKED(9) CONST;
000143170712         prmSeriePartita CHAR(4) CONST;
000144170712         prmLibroNA CHAR(3) CONST;
000145170712       END-PR ;
000146170710
000147170711       // --------------------------------------------------
000148170711       // Prototype for procedure: YIVASPLIT_EndDocumento
000149170711       // --------------------------------------------------
000150170711
000151170711       DCL-PR YIVASPLIT_EndDocumento INT(10);
000152170711       END-PR ;
000153170711
000154170711       // --------------------------------------------------
000155170711       // Prototype for procedure: YIVASPLIT_GetLibroSplit
000156170711       // --------------------------------------------------
000157170711
000158170711       DCL-PR YIVASPLIT_GetLibroSplit CHAR(3);
000159170711       END-PR ;
000160170711
000161170711       // --------------------------------------------------
000162170711       // Prototype for procedure: YIVASPLIT_GetFraseSplit
000163170711       // --------------------------------------------------
000164170711
000165170711       DCL-PR YIVASPLIT_GetFraseSplit VARCHAR(255);
000166170711       END-PR ;
000167170711
000168170711       // --------------------------------------------------
000169170711       // Prototype for procedure: YIVASPLIT_AddGirocontoIva
000170170711       // --------------------------------------------------
000171170711
000172170711       DCL-PR YIVASPLIT_AddGirocontoIva INT(10);
000173170711         prmRifInt CHAR(9) CONST;
000174170711         prmSubNum PACKED(9:0) CONST;
000175170712         prmCommit IND CONST;
000176170713         prmDataScadenza DATE CONST OPTIONS(*NOPASS);
000177170713       END-PR ;
000178170711
000179170714       // --------------------------------------------------
000180170714       // Prototype for procedure: YIVASPLIT_IsDocumentoSplitByLibro
000181170714       // --------------------------------------------------
000182170714
000183170714       DCL-PR YIVASPLIT_IsLibroIvaSplit IND;
000184170714         prmSocieta CHAR(3) CONST;
000185170714         prmLibro CHAR(3) CONST;
000186170714       END-PR ;
000187170714
