000100170831       // --------------------------------------------------
000101170831       // Prototype for procedure: YIVACHKP_Init
000102170831       // --------------------------------------------------
000103170831
000104170831       DCL-PR YIVACHKP_Init INT(10);
000105170831         prmKpjba LIKEDS(kpjba) OPTIONS(*NOPASS);
000106170831       END-PR ;
000107170831
000108170831       // --------------------------------------------------
000109170831       // Prototype for procedure: YIVACHKP_Finalize
000110170831       // --------------------------------------------------
000111170831
000112170831       DCL-PR YIVACHKP_Finalize ;
000113170831       END-PR ;
000114170831
000115170831       // --------------------------------------------------
000116170831       // Prototype for procedure: YIVACHKP_NewControllo
000117170831       // --------------------------------------------------
000118170831
000119170831       DCL-PR YIVACHKP_NewControllo INT(10);
000120170905         prmGraMinErr ZONED(2) CONST;
000121170905         prmAnno ZONED(4:0) CONST;
000122170831         prmRegistro CHAR(1) CONST;
000123170831         prmLibro1 CHAR(3) CONST;
000124170831         prmLibro2 CHAR(3) CONST;
000125170831         prmSocieta CHAR(3) CONST;
000127170831       END-PR ;
000128170831
000129170831       // --------------------------------------------------
000130170831       // Prototype for procedure: YIVACHKP_RunControllo
000131170831       // --------------------------------------------------
000132170831
000133170831       DCL-PR YIVACHKP_RunControllo INT(10);
000134170831       END-PR ;
000135170831
000136170901       // --------------------------------------------------
000137170901       // Prototype for procedure: YIVACHKP_EndControllo
000138170901       // --------------------------------------------------
000139170901
000140170901       DCL-PR YIVACHKP_EndControllo INT(10);
000141170901       END-PR ;
000142170901
000143170901       // --------------------------------------------------
000144170901       // Prototype for procedure: YIVACHKP_GetErroreProtocollo
000145170901       // --------------------------------------------------
000146170901
000147170901       DCL-PR YIVACHKP_GetErroreProtocollo INT(10);
000148170901         prmSocieta CHAR(3);
000149170901         prmSerie CHAR(4);
000150170907         prmIdErrore CHAR(7);
000151170907         prmGravita ZONED(2);
000152170901         prmMessaggio VARCHAR(255);
000153170901       END-PR ;
000154170901
000155170908       // --------------------------------------------------
000156170908       // Prototype for procedure: YIVACHKP_Get9Messaggi
000157170908       // --------------------------------------------------
000158170908
000159170908       DCL-PR YIVACHKP_Get9Messaggi LIKEDS(yMsg9);
000161170908       END-PR ;
000162170908
