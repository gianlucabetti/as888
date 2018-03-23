000100970526     H*------------------------------------------------------------------------*
000200040422     H* UTILITY RI-ASSEGNAZIONE ABILITAZIONI STANDARD X UTENTI SGI + DELETE OUTQ SGI NN NECESSARIE
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800040422     Ftivss01l  IF   E           K DISK
000900040422     Ftntbe01l  IF   E           K DISK
001000010917     FPRTF198   O    F  198        PRINTER
001100970526     D*------------------------------------------------------------------------*
001200040422     D*------------
001300040422     D* DS ESTERNE
001400040422     D*------------
001500040422     D DISV          E DS
001600001020     D*------------
001700010313     D* VARIABILI DI WRK
001800010917     D*------------
001900010917     D rigaPRT         s            190    inz(*blanks)
002000040422     D skISVOK         s              2    dim(20) inz(*blanks)
002100040422     D skISVOKTYP      s              1    dim(20) inz(*blanks)
002200040422     D skISVSGIUS      s              1    dim(20) inz(*blanks)
002300040422     D skISVSGI        s              2    dim(20) inz(*blanks)
002400040422     D depSUN          s                   like(vssSUN) inz(*blanks)
002500040422     D flgISV          s              1    inz(*blanks)
002600040422     D flgSGI          s              1    inz(*blanks)
002700040422     D cmd             s            500
002800040422     D api             s              1    inz('''')
002900970526     C*------------------------------------------------------------------------*
003000970526     C* MAIN LINES
003100970526     C*------------------------------------------------------------------------*
003200970526     C*
003300010917     C* ESECUZIONE OPERAZIONI
003400010917     C                   EXSR      EXEOPE
003500970526     C* FINE PGM
003600970526     C                   SETON                                        LR
003700010917     C*------------------------------------------------------------------------*
003800010917     C* EXEOPE - ESEGUE LE OPERAZIONI
003900010917     C*------------------------------------------------------------------------*
004000010917     C     EXEOPE        BEGSR
004100010917     C*
004200040422     C     *loval        SETLL     tivss01l
004300040422     C                   READ      tivss01l
004400040422     C                   EVAL      depSUN = vssSUN
004500040422     C*
004600040422     C* Inizializzo il flag d wrk che indica d resettare i parametri d STRATEGI e
004700040422     C* deletare la OUTQ dello SGIUS relativo
004800040422     C                   EVAL      flgISV = 'N'
004900040422     C*
005000040422     C* Inizializzo flag che indica se trattasi d utente STRATEGI (e nn ad esempio firrizio x FTP)
005100040422     C                   EVAL      flgSGI = 'N'
005200010917     C*
005300040422     C                   DOW       not %eof(tivss01l)
005400040422     C*
005500040422     C* Verifico se x il cliente/SGIUS sono abilitati servizi che richiedono particolari parametri
005600040422     C* d STRATEGI e la OUTQ dello SGIUS relativo
005700040422     C                   IF        depSUN = vssSUN
005800040422     C     vssISV        LOOKUP    skISVSGI                               50
005900040422     C                   IF        %found
006000040422     C                   EVAL      flgSGI = 'S'
006100040422     C                   Z-ADD     1             jS
006200040422     C     vssISV        LOOKUP    skISVOK(jS)                            51
006300040422     C                   IF        %found
006400040422     C                   EVAL      flgISV = 'S'
006500040422     C     skISVOKTYP(jS)LOOKUP    skISVSGIUS                             52
006600040422     C                   IF        not %found
006700040422     C                   ADD       1             jSSGIUS
006800040422     C                   EVAL      skISVSGIUS(jSSGIUS) = skISVOKTYP(jS)
006900040422     C                   ENDIF
007000040422     C                   ENDIF
007100040422     C                   ENDIF
007200040422     C*
007300040422     C                   ELSE
007400040422     C*
007500040422     C* Solo se trattasi d utente STRATEGI => proseguo con l'elaborazione
007600040422     C                   IF        flgSGI = 'S'
007700040422     C                   EXSR      EXEMODSGIUS
007800040422     C                   EXCEPT    listamod
007900040422     C                   ENDIF
008000040422     C*
008100040422     C* Inizializzo e salvo i nuovi depositi x il SGIUS corrente
008200040422     C                   EVAL      depSUN = vssSUN
008300040422     C                   EVAL      flgISV = 'N'
008400040422     C                   EVAL      flgSGI = 'N'
008500040422     C                   CLEAR                   skISVSGIUS
008600040422     C                   Z-ADD     *zeros        jSSGIUS           3 0
008700040422     C*
008800040422     C* Eseguo test x utente SGI corrente
008900040422     C     vssISV        LOOKUP    skISVSGI                               50
009000040422     C                   IF        %found
009100040422     C                   EVAL      flgSGI = 'S'
009200040422     C                   Z-ADD     1             jS
009300040422     C     vssISV        LOOKUP    skISVOK(jS)                            51
009400040422     C                   IF        %found
009500040422     C                   EVAL      flgISV = 'S'
009600040422     C     skISVOKTYP(jS)LOOKUP    skISVSGIUS                             52
009700040422     C                   IF        not %found
009800040422     C                   ADD       1             jSSGIUS
009900040422     C                   EVAL      skISVSGIUS(jSSGIUS) = skISVOKTYP(jS)
010000040422     C                   ENDIF
010100040422     C                   ENDIF
010200040422     C                   ENDIF
010300040422     C                   ENDIF
010400040422     C*
010500040422     C                   READ      tivss01l
010600010917     C                   ENDDO
010700040422     C*
010800040422     C* Al termine del file verifico se l'ultimo record letto era da trattare => procedo
010900040422     C                   IF        depSUN = vssSUN AND
011000040422     C                             flgSGI = 'S'
011100040422     C                   EXSR      EXEMODSGIUS
011200040422     C                   EXCEPT    listamod
011300040422     C                   ENDIF
011400010917     C*
011500010917     C                   ENDSR
011600010810     C*------------------------------------------------------------------------*
011700040422     C* EXEMODSGIUS - OPERAZIONI DI MODIFICA AD USO EDP
011800010810     C*------------------------------------------------------------------------*
011900040422     C     EXEMODSGIUS   BEGSR
012000040428     C*
012100040428     C* X elaborazione "semplice" (ovvero parametro in ingresso = *blanks) eseguo solo
012200040428     C* ma modifica del parametro ALWWEB = *YES dell'utente SGI
012300040428     C                   IF        TIPLAN = *blanks
012400040428     C* Costrisco la stringa d comando x impostare il parametro ALWWEB = *YES
012500040428     C                   EVAL      CMD = 'STRATEGI/CHGSGIUSR ' +
012600040428     C                                               depSUN + '  ' +
012700040428     C                                 'ALWWEB(' + api + '*YES' + api + ') '
012800040428     C                   ELSE
012900040422     C*
013000040422     C* Verifico il valore del flag e d conseguenza costruisco il comando da eseguire
013100040422     C                   IF        flgISV = 'S'
013200040422     C*
013300040422     C* Effettuo una 1° chiamata standard x resettare tutti i parametri
013400040422     C*
013500040422     C* Costrisco la stringa d comando x impostare i parametri adeguati (in questo caso gli *STD)
013600040422     C                   EVAL      CMD = 'STRATEGI/CHGSGIUSR ' +
013700040422     C                                               depSUN + '  ' +
013800040428     C                                 'ALWWEB(' + api + '*YES' + api + ') ' +
013900040428     C                                 'ALWGUI(' + api + '*NO'  + api + ') ' +
014000040428     C                                 'ALWPKT(' + api + '*NO'  + api + ') ' +
014100040428     C                                 'ALWRMT(' + api + '*NO'  + api + ') ' +
014200040428     C                                 'ALWEMU(' + api + '*NO'  + api + ') ' +
014300040428     C                               'ALWSPOOL(' + api + '*NO'  + api + ') ' +
014400040428     C                                'ALWFILE(' + api + '*NO'  + api + ') ' +
014500040428     C                             'ALWRCVFILE(' + api + '*NO'  + api + ') ' +
014600040428     C                                 'ALWHSM(' + api + '*NO'  + api + ') '
014700040422     C*
014800040422     C                   CALL(e)   'TIS730C'
014900040422     C                   PARM                    cmd
015000040422     C*
015100040422     C* Effettuo una 2° chiamata x impostare solo i parametri occorrenti
015200040422     C*
015300040422     C* Costrisco la stringa d comando x impostare i parametri adeguati
015400040422     C                   EVAL      CMD = 'STRATEGI/CHGSGIUSR ' +
015500040422     C                                               depSUN + '  ' +
015600040422     C                                 'ALWWEB(' + api + '*YES' + api + ') ' +
015700040422     C                                 'ALWGUI(' + api + '*NO'  + api + ') ' +
015800040422     C                                 'ALWPKT(' + api + '*NO'  + api + ') ' +
015900040422     C                                 'ALWRMT(' + api + '*NO'  + api + ') ' +
016000040422     C                                 'ALWEMU(' + api + '*NO'  + api + ') ' +
016100040422     C                                 'ALWHSM(' + api + '*NO'  + api + ') '
016200040422     C                   EVAL      rigaPRT = depSUN + ' - '
016300040422     C*
016400040422     C* Verifico tutti i servizi "particolari" da attivare
016500040422     C                   Z-ADD     1             i                 3 0
016600040422     C                   DOW       i <= %elem(skISVSGIUS)
016700040422     C                   IF        skISVSGIUS(i) <> *blanks
016800040422     C*
016900040422     C* Gestisco i vari parametri diversi da abilitare a seconda del servizio
017000040422     C                   IF        skISVSGIUS(i) = 'U'                          * UPLOAD
017100040422     C                   EVAL      CMD = %trim(CMD) + ' ' +
017200040422     C                             'ALWRCVFILE(' + api + '*YES' + api + ') '
017300040422     C                   EVAL      rigaPRT = %trim(rigaPRT) + ' UPLOAD '
017400040422     C                   ENDIF
017500040422     C                   IF        skISVSGIUS(i) = 'D'                          * DOWNLOAD
017600040422     C                   EVAL      CMD = %trim(CMD) + ' ' +
017700040422     C                                'ALWFILE(' + api + '*YES' + api + ') '
017800040422     C                   EVAL      rigaPRT = %trim(rigaPRT) + ' DOWNLOAD '
017900040422     C                   ENDIF
018000040422     C                   IF        skISVSGIUS(i) = 'S'                          * SERVIZIO T1
018100040422     C                   EVAL      CMD = %trim(CMD) + ' ' +
018200040422     C                              ' ALWSPOOL(' + api + '*YES' + api + ') '
018300040422     C                   EVAL      rigaPRT = %trim(rigaPRT) + ' OUTQ '
018400040422     C                   ENDIF
018500040422     C                   ENDIF
018600040422     C                   ADD       1             i
018700040422     C                   ENDDO
018800040422     C*
018900040422     C                   ELSE
019000040422     C* Costrisco la stringa d comando x impostare i parametri adeguati (in questo caso gli *STD)
019100040422     C                   EVAL      CMD = 'STRATEGI/CHGSGIUSR ' +
019200040422     C                                               depSUN + '  ' +
019300040428     C                                 'ALWWEB(' + api + '*YES' + api + ') ' +
019400040428     C                                 'ALWGUI(' + api + '*NO'  + api + ') ' +
019500040428     C                                 'ALWPKT(' + api + '*NO'  + api + ') ' +
019600040428     C                                 'ALWRMT(' + api + '*NO'  + api + ') ' +
019700040428     C                                 'ALWEMU(' + api + '*NO'  + api + ') ' +
019800040428     C                               'ALWSPOOL(' + api + '*NO'  + api + ') ' +
019900040428     C                                'ALWFILE(' + api + '*NO'  + api + ') ' +
020000040428     C                             'ALWRCVFILE(' + api + '*NO'  + api + ') ' +
020100040428     C                                 'ALWHSM(' + api + '*NO'  + api + ') '
020200040422     C*
020300040422     C                   EVAL      rigaPRT = depSUN + ' - Impostati parametri '+
020400040422     C                                       'standard.'
020500040422     C                   ENDIF
020600040428     C                   ENDIF
020700010917     C*
020800040422     C                   CALL(e)   'TIS730C'
020900040422     C                   PARM                    cmd
021000040422     C                   IF        %error OR
021100040422     C                             %trim(cmd) = '*ERR'
021200040422     C                   EVAL      rigaPRT = %trim(rigaPRT) + ' *ERRORE'
021300040422     C                   ENDIF
021400010810     C*
021500010810     C                   ENDSR
021600040422     C*------------------------------------------------------------------------*
021700040422     C* CARTAB - CARICAMENTO TABELLE                                           *
021800040422     C*------------------------------------------------------------------------*
021900040422     C     CARTAB        BEGSR
022000040422     C*
022100040422     C* Reperisco dalla tabella "ISV" i servizi x i quali è necessaria la outq su AS777 x lo SGIUS
022200040422     C                   z-add     *zeros        jS                3 0
022300040422     C                   z-add     *zeros        jG                3 0
022400040422     C     'ISV'         setll     tntbe01l
022500040422     C                   if        %equal(tntbe01l)
022600040422     C     'ISV'         reade     tntbe01l
022700040422     C                   dow       not %eof(tntbe01l)
022800040422     C                   eval      DISV = tbeUNI
022900040422     C                   if        §ISVSGI1 <> *blanks
023000040422     C                   add       1             jS
023100040422     C                   eval      skISVOK(jS) = tbeKE1
023200040422     C                   eval      skISVOKTYP(jS) = §ISVSGI1
023300040422     C                   endif
023400040422     C                   if        §ISVPROC = 'S'
023500040422     C                   add       1             jG
023600040422     C                   eval      skISVSGI(jG) = tbeKE1
023700040422     C                   endif
023800040422     C     'ISV'         reade     tntbe01l
023900040422     C                   enddo
024000040422     C                   endif
024100040422     C*
024200040422     C                   ENDSR
024300970526     C*------------------------------------------------------------------------*
024400001020     C* *INZSR - OPERAZIONI INIZIALI
024500970526     C*------------------------------------------------------------------------*
024600970526     C     *INZSR        BEGSR
024700040428     C*
024800040428     C     *ENTRY        PLIST
024900040428     C                   PARM                    TIPLAN            1
025000040422     C*
025100040422     C* CARICO LE TABELLE OCCORRENTI
025200040422     C                   EXSR      CARTAB
025300970526     C*
025400970526     C                   ENDSR
025500010917
025600010918     OPRTF198   E            listamod    1
025700010917     O                       rigaPRT             +1
