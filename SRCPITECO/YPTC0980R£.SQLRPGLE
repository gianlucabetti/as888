000100141217      //************************************************************************
000101141217      //
000102141217      // Questo programma esegue la riconciliazione delle partite del capo
000103141217      // conto 000159 effetti in portafoglio.
000107141217      //
000108141217      //************************************************************************
000109141217
000110141217     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('PJCBND')
000111141217
000112141217      //************************************************************************
000113141217      //
000114141217      // Strutture dati.
000115141217      //
000116141217      //************************************************************************
000117141217
000119141217     D kpjba         E DS                  QUALIFIED
000120141218     D pas             DS                  QUALIFIED OCCURS(255)
000121141218     D  sys                           3P 0
000122141218     D  nrAsReg                       9P 0
000123141218     D  nrRigaM                       5P 0
000124141218     D  nrRiga                        5P 0
000125141218     D  dtPar                          D   DATFMT(*ISO)
000126141218     D  nrPar                         9P 0
000127141218     D  serPar                        4A
000140141217     D this            DS                  QUALIFIED INZ
000141141217     D  i                            10I 0
000142141217     D  societa                       3A
000152141217
000153141217      //************************************************************************
000154141217      //
000155141217      // Prototipi.
000156141217      //
000157141217      //************************************************************************
000158141217
000163141218     D Proj_ChgIdPartita...
000164141217     D                 PR                  EXTPROC('NDMVC404')
000165141217     D  sys                           3P 0
000166141217     D  nrAsReg                       9P 0
000167141217     D  nrRigaM                       5P 0
000168141217     D  nrRiga                        5P 0
000169141217     D  nrPar                         9P 0
000170141217     D  dtPar                          D
000171141217     D  serPar                        4A
000172141217     D  commit                        1A   CONST
000189141218
000190141217      //************************************************************************
000191141217      //
000192141217      // Parametri.
000193141217      //
000194141217      //************************************************************************
000195141217
000196141217     C     *ENTRY        PLIST
000197141217     C                   PARM                    kpjba
000198141217
000199141217      //************************************************************************
000200141217      //
000201141217      // Main.
000202141217      //
000203141217      //************************************************************************
000204141217
000205141217      /FREE
000206141217
000207141217       *INLR = *ON;
000208141217
000209141217       this.societa = %SUBST(kpjba.kpjbu : 1 : 3);
000210141217
000211141218       RunDare();
000212141218       RunAvere();
000580141217
000581141217      /END-FREE
000582141218
000583141218     P*--------------------------------------------------
000584141218     P* Procedure name: RunDare
000585141218     P* Purpose:        Elaborazione movimenti dare (effetti presentati)
000586141218     P*                 A tutti gli effetti presentati imposto
000587141218     P*                 data partita = data distinta
000588141218     P*                 numero partita = numero distinta
000589141218     P*                 serie partita = banca
000590141218     P* Returns:        Esito.
000591141218     P*--------------------------------------------------
000592141218     P RunDare         B
000593141218     D RunDare         PI            10I 0
000594141218
000595141218     D retField        S             10I 0
000604141218
000605141218      /FREE
000606141218
000607141218       EXEC SQL
000608141218         DECLARE d NO SCROLL CURSOR WITH HOLD FOR
000609141218           WITH ppa (societa, ctb, kcc, ksc, dtPar, nrPar, serPar) AS
000610141218                    ( SELECT ppaSocieta, ppaCtb, ppaKcc, ppaKsc
000611141218                           , ppaDtPar, ppaNrPar, ppaSerPar
000612141218                        FROM ndppa00f
000613141218                       WHERE ppaSocieta = :this.societa
000614141218                         AND ppaCtb = 'CG'
000615141218                         AND ppaKcc = '000159'
000616141218                         AND ppaDtPar > '2013-11-30'
000617150210                         AND ppaParChiu = '0'
000618150210                         AND ( ppaNrPar = 1 AND ppaSerPar = 'EUR'
000619150210                               OR ppaSerPar = '' )
000620141218                    )
000621141218           SELECT ndpas00f.pasSys, ndpas00f.pasNrAsReg, ndpas00f.pasNrRigaM
000622141218                , ndpas00f.pasNrRiga
000623141218                , ndefa00f.efaDtDist, ndefa00f.efaNrDist, ndefa00f.efaBanca
000626141218             FROM ppa
000627141218             JOIN ndpas00f ON ppa.societa = ndpas00f.pasSocieta
000628141218                          AND ppa.ctb = ndpas00f.pasCtb
000629141218                          AND ppa.kcc = ndpas00f.pasKcc
000630141218                          AND ppa.ksc = ndpas00f.pasKsc
000631141218                          AND ppa.dtPar = ndpas00f.pasDtPar
000632141218                          AND ppa.nrPar = ndpas00f.pasNrPar
000633141218                          AND ppa.serPar = ndpas00f.pasSerPar
000634141218             JOIN ndefa00f ON ndpas00f.pasSys = ndefa00f.efaSys
000635141218                          AND ndpas00f.pasNrAsReg = ndefa00f.efaNrAsReg
000636141218            WHERE ndpas00f.pasDare = 1
000637141218              AND ndefa00f.efaStatus = '4' -- Effetto presentato
000638150210              AND ndefa00f.efaBanca <> ''
000639150210              AND (( ndpas00f.pasDtPar <> ndefa00f.efaDtDist OR
000640150210                     ndpas00f.pasNrPar <> ndefa00f.efaNrDist OR
000641150210                     ndpas00f.pasSerPar <> ndefa00f.efaBanca )
000642150210                   OR
000643150210                   ( ndpas00f.pasDtPar = ndefa00f.efaDtDist AND
000644150210                     ndpas00f.pasNrPar = ndefa00f.efaNrDist AND
000645150210                     ndpas00f.pasSerPar = '' ))
000646141218             SKIP LOCKED DATA
000647141218              FOR FETCH ONLY
000648141218       ;
000649141218
000650141218       EXEC SQL OPEN d;
000651141218
000652141218       SELECT;
000653141218         WHEN sqlCode < *ZERO;
000654141218           DUMP(A);
000655141218           RETURN sqlCode;
000656141218       ENDSL;
000657141218
000658141218       DOU sqlCode < *ZERO;
000659141218
000660141218         EXEC SQL FETCH NEXT FROM d FOR 255 ROWS INTO :pas;
000661141218
000662141218         SELECT;
000663141218           WHEN sqlCode = 100;
000664141218             LEAVE;
000665141218           WHEN sqlCode < *ZERO;
000666141218             DUMP(A);
000667141218             retField = sqlCode;
000668141218             LEAVE;
000669141218         ENDSL;
000670141218
000671141218         FOR this.i = 1 TO sqlErrD(3);
000672141218           %OCCUR(pas) = this.i;
000673141218           Proj_ChgIdPartita( pas.sys : pas.nrAsReg : pas.nrRigaM : pas.nrRiga
000674141218                            : pas.nrPar : pas.dtPar : pas.serPar : *ON );
000675141218         ENDFOR;
000676141218
000677141218         IF sqlErrD(5) = 100;
000678141218           LEAVE;
000679141218         ENDIF;
000680141218
000681141218       ENDDO;
000682141218
000683141218       EXEC SQL CLOSE d;
000684141218
000685141218       RETURN retField;
000686141218
000687141218      /END-FREE
000688141218     P RunDare         E
000689141218
000690141218
000691141218     P*--------------------------------------------------
000692141218     P* Procedure name: RunAvere
000693141218     P* Purpose:        Elaborazione movimenti avere (effetti presentati)
000694141218     P*                 A tutti i movimenti di presentazione inseriti da Piteco
000695141218     P*                 imposto nella serie partita l'ID banca.
000696141218     P* Returns:        Esito.
000697141218     P*--------------------------------------------------
000698141218     P RunAvere        B
000699141218     D RunAvere        PI            10I 0
000700141218
000701141218     D retField        S             10I 0
000702141218
000703141218      /FREE
000704141218
000705141218       EXEC SQL
000706141218         DECLARE a NO SCROLL CURSOR WITH HOLD FOR
000707141218           WITH ppa (societa, ctb, kcc, ksc, dtPar, nrPar, serPar) AS
000708141218                    ( SELECT ppaSocieta, ppaCtb, ppaKcc, ppaKsc
000709141218                           , ppaDtPar, ppaNrPar, ppaSerPar
000710141218                        FROM ndppa00f
000711141218                       WHERE ppaSocieta = :this.societa
000712141218                         AND ppaCtb = 'CG'
000713141218                         AND ppaKcc = '000159'
000714141218                         AND ppaDtPar > '2013-11-30'
000715141218                         AND ppaSerPar = ''
000716141218                         AND ppaParChiu = '0'
000717141218                    )
000718141218           SELECT ndpas00f.pasSys, ndpas00f.pasNrAsReg, ndpas00f.pasNrRigaM
000719141218                , ndpas00f.pasNrRiga, ndpas00f.pasDtPar, ndpas00f.pasNrPar
000720141218                , SUBSTR(ndreg00f.regKscCpD, 5)
000721141218             FROM ppa
000722141218             JOIN ndpas00f ON ppa.societa = ndpas00f.pasSocieta
000723141218                          AND ppa.ctb = ndpas00f.pasCtb
000724141218                          AND ppa.kcc = ndpas00f.pasKcc
000725141218                          AND ppa.ksc = ndpas00f.pasKsc
000726141218                          AND ppa.dtPar = ndpas00f.pasDtPar
000727141218                          AND ppa.nrPar = ndpas00f.pasNrPar
000728141218                          AND ppa.serPar = ndpas00f.pasSerPar
000729141218             JOIN ndreg00f ON ndpas00f.pasSys = ndreg00f.regSys
000730141218                          AND ndpas00f.pasNrAsReg = ndreg00f.regNrAsReg
000731141218            WHERE ndpas00f.pasAvere = 1
000732141218             SKIP LOCKED DATA
000733141218              FOR FETCH ONLY
000734141218       ;
000735141218
000736141218       EXEC SQL OPEN a;
000737141218
000738141218       SELECT;
000739141218         WHEN sqlCode < *ZERO;
000740141218           DUMP(A);
000741141218           RETURN sqlCode;
000742141218       ENDSL;
000743141218
000744141218       DOU sqlCode < *ZERO;
000745141218
000746141218         EXEC SQL FETCH NEXT FROM a FOR 255 ROWS INTO :pas;
000747141218
000748141218         SELECT;
000749141218           WHEN sqlCode = 100;
000750141218             LEAVE;
000751141218           WHEN sqlCode < *ZERO;
000752141218             DUMP(A);
000753141218             retField = sqlCode;
000754141218             LEAVE;
000755141218         ENDSL;
000756141218
000757141218         FOR this.i = 1 TO sqlErrD(3);
000758141218           %OCCUR(pas) = this.i;
000759141218           Proj_ChgIdPartita( pas.sys : pas.nrAsReg : pas.nrRigaM : pas.nrRiga
000760141218                            : pas.nrPar : pas.dtPar : pas.serPar : *ON );
000761141218         ENDFOR;
000762141218
000763141218         IF sqlErrD(5) = 100;
000764141218           LEAVE;
000765141218         ENDIF;
000766141218
000767141218       ENDDO;
000768141218
000769141218       EXEC SQL CLOSE a;
000772141218
000773141218       RETURN retField;
000774141218
000775141218      /END-FREE
000776141218     P RunAvere        E
000777141218
