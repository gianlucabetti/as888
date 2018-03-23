000100091228       //==============================================================
000200160310       // Driver scrittura WAS7IFS0F - DOCFLOW DATACERTA
000300091228       //==============================================================
000400091223
000500160310     H decedit('0,') datedit(*dmy/) option(*nodebugio)
000600160310     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000700160310     H BNDDIR('UBBNDDIR')
000800091223
000900091223       //--------------------------------------------------------------
001000160310       // Dichiarazione file
001100091223       //--------------------------------------------------------------
001200100112
001300160310     FTNTBE01L  IF   E           K DISK
001400160310     FTIMAR01L  IF   E           K DISK
001500130403     FWAS7IFS0F O    E             DISK    usropn
001600160310
001700160310
001800160310       //--------------------------------------------------------------
001900160310       // Dichiarazioni DS esterne
002000160310       //--------------------------------------------------------------
002100160310     D DAOA          e ds                  inz
002200160310
002300160310       // - Status
002400160310     D Psds           sds
002500160310     D   SDSpgm          *proc
002600160310     D   JobName             244    253                                         Job name
002700160310     D   JobUser             254    263                                         User name
002800160310     D   JobNumber           264    269s 0                                      Job number
002900160310
003000160310
003100091223       //--------------------------------------------------------------
003200160310       // Definizione variabili
003300091223       //--------------------------------------------------------------
003400121106
003500121106       // Campi di comodo
003600160310     D pathINPUT       S            255    inz
003700160310     D pathOUTPUT      S            255    inz
003800160310     D pathSUBFLR      S            255    inz
003900160310     D pathPROD        S            255    inz
004000160310     D pathTEST        S            255    inz
004100160311     D KTBECOD         S                   like(TBECOD) inz
004200160311     D KTBEKE1         S                   like(TBEKE1) inz
004300160311     D KTBEKE2         S                   like(TBEKE2) inz
004400160311     D wExtFile        S             11    inz
004500160310
004600160310
004700160310
004800160310       //--------------------------------------------------------------
004900160310       // MAIN
005000160310       //--------------------------------------------------------------
005100160310
005200160310     C*
005300160310     C* pIn_TLA
005400160310     C* se ' ' elaborazione (singola) + chiusura
005500160310     C* se 'C' solo chiusura
005600160310     C* se 'R' solo elaborazione (prevederebbe un lancio finale in 'C')
005700160310     C*
005800160310     C* pIn_TIPS
005900160310     C* è la regola indicata sullo spool
006000160310     C*
006100160310     C* pIn_NomeFile
006200160310     C* è il nome del file (completo di estensione) indicato sullo spool
006300160310     C*
006400160310     C* pOut_Esito
006500160310     C* se '1' allora OK
006600160310     C* se '2' allora KO
006700160310     C*
006800160310     C*
006900160310     C* Ricevimento parametri
007000160310     C     *ENTRY        PLIST
007100160310     C                   PARM                    pIn_TLA           1
007200160310     C                   PARM                    pIn_TIPS          3
007300160311     C                   PARM                    pIn_NomeFile     60
007400160311     C                   PARM                    pIn_ExtFile      11
007500160310     C                   PARM                    pOut_Esito        1
007600160310     C*
007700160310     C* Innanzitutto verifico il tipo lancio richiesto in ingresso
007800160310     C                   IF        pIn_TLA = 'C'
007900160310     C                   EXSR      sr_RoutEnd
008000160310     C                   ELSE
008100160311     C*
008200160311     C* Inizializzazioni
008300160311     C                   CLEAR                   pathINPUT
008400160311     C                   CLEAR                   pathOUTPUT
008500160311     C                   CLEAR                   pathSUBFLR
008600160311     C                   CLEAR                   pathPROD
008700160311     C                   CLEAR                   pathTEST
008800160311     C                   CLEAR                   wExtFile
008900160310     C*
009000160310     C* Reperisco la DATA e ORA corrente
009100160310     C                   TIME                    wn14             14 0
009200160310     C                   MOVEL     wn14          ORACOR            6 0          *ORA
009300160310     C                   Z-ADD     *zeros        DATCOR            8 0          *DATA CORRENTE AA/M/
009400160310     C                   EVAL      DATCOR  = %dec(%date() : *ISO)
009500160310     C                   Z-ADD     *zeros        DATAORA          14 0
009600160310     C                   EVAL      DATAORA = DATCOR*1000000+ORACOR
009700160311     C*
009800160311     C* Verifico se nel estensione è già presente il "."
009900160311     C                   IF        %subst(%trim(pIn_ExtFile):1:1) <> '.'
010000160311     C                   EVAL      pIn_ExtFile = '.'+%trim(pIn_ExtFile)
010100160311     C                   ENDIF
010200160310     C*
010300160310     C* Definizione chiavi di lettura
010400160310     C*
010500160310     C* TNTBE01L - Parziale
010600160310     C     KEYTBE01_P    KLIST
010700160310     C                   KFLD                    KTBECOD
010800160310     C                   KFLD                    KTBEKE1
010900160310     C                   KFLD                    KTBEKE2
011000160310     C*
011100160310     C* Reperisco la definizione degli ambienti da gestire dalla tabella di procedura preposta
011200160310     C                   MOVEL(p)  'AOA'         KTBECOD
011300160310     C                   MOVEL(p)  'TIS7P2SR4'   KTBEKE1
011400160310     C                   MOVEL(p)  DATCOR        KTBEKE2
011500160310     C                   CLEAR                   DAOA
011600160310     C                   SETOFF                                       33
011700160310     C     KEYTBE01_P    SETGT     tntbe01l
011800160310     C                   IF        %found(tntbe01l)
011900160310     C                   DOW       not %eof(tntbe01l) AND
012000160310     C                             *IN33 = *OFF
012100160310     C                   READP     tntbe01l
012200160310     C                   IF        TBEKE1  = KTBEKE1
012300160310     C                   IF        TBEKE2 <= KTBEKE2 AND
012400160310     C                             TBEATB  = *blanks
012500160310     C                   SETON                                        33
012600160310     C                   ENDIF
012700160310     C                   ELSE
012800160310     C                   LEAVE
012900160310     C                   ENDIF
013000160310     C                   ENDDO
013100160310     C                   ENDIF
013200160310     C*
013300160310     C                   IF        *IN33
013400160310     C                   MOVEL(P)  TBEUNI        DAOA
013500160310     C                   ENDIF
013600160310     C*
013700160310     C* Reperisco i parametri di procedura dalla tabella di procedura preposta
013800160310     C                   MOVEL(p)  'AOP'         KTBECOD
013900160310     C                   EVAL      KTBEKE1 = 'DATACERT_INPUT'
014000160310     C                   MOVEL(p)  DATCOR        KTBEKE2
014100160310     C                   CLEAR                   pathINPUT
014200160310     C                   SETOFF                                       34
014300160310     C     KEYTBE01_P    SETGT     tntbe01l
014400160310     C                   IF        %found(tntbe01l)
014500160310     C                   DOW       not %eof(tntbe01l) AND
014600160310     C                             *IN34 = *OFF
014700160310     C                   READP     tntbe01l
014800160310     C                   IF        TBEKE1  = KTBEKE1
014900160310     C                   IF        TBEKE2 <= KTBEKE2 AND
015000160310     C                             TBEATB  = *blanks
015100160310     C                   SETON                                        34
015200160310     C                   ENDIF
015300160310     C                   ELSE
015400160310     C                   LEAVE
015500160310     C                   ENDIF
015600160310     C                   ENDDO
015700160310     C                   ENDIF
015800160310     C*
015900160310     C                   IF        *IN34
016000160310     C                   EVAL      pathINPUT = %trim(tbeUNI)
016100160310     C                   ENDIF
016200160310     C*
016300160310     C* Reperisco i parametri di procedura dalla tabella di procedura preposta
016400160310     C                   MOVEL(p)  'AOP'         KTBECOD
016500160310     C                   EVAL      KTBEKE1 = 'DATACERT_PROD'
016600160310     C                   MOVEL(p)  DATCOR        KTBEKE2
016700160310     C                   CLEAR                   pathPROD
016800160310     C                   SETOFF                                       35
016900160310     C     KEYTBE01_P    SETGT     tntbe01l
017000160310     C                   IF        %found(tntbe01l)
017100160310     C                   DOW       not %eof(tntbe01l) AND
017200160310     C                             *IN35 = *OFF
017300160310     C                   READP     tntbe01l
017400160310     C                   IF        TBEKE1  = KTBEKE1
017500160310     C                   IF        TBEKE2 <= KTBEKE2 AND
017600160310     C                             TBEATB  = *blanks
017700160310     C                   SETON                                        35
017800160310     C                   ENDIF
017900160310     C                   ELSE
018000160310     C                   LEAVE
018100160310     C                   ENDIF
018200160310     C                   ENDDO
018300160310     C                   ENDIF
018400160310     C*
018500160310     C                   IF        *IN35
018600160310     C                   EVAL      pathPROD  = %trim(tbeUNI)
018700160310     C                   ENDIF
018800160310     C*
018900160310     C* Reperisco i parametri di procedura dalla tabella di procedura preposta
019000160310     C                   MOVEL(p)  'AOP'         KTBECOD
019100160310     C                   EVAL      KTBEKE1 = 'DATACERT_TEST'
019200160310     C                   MOVEL(p)  DATCOR        KTBEKE2
019300160310     C                   CLEAR                   pathTEST
019400160310     C                   SETOFF                                       36
019500160310     C     KEYTBE01_P    SETGT     tntbe01l
019600160310     C                   IF        %found(tntbe01l)
019700160310     C                   DOW       not %eof(tntbe01l) AND
019800160310     C                             *IN36 = *OFF
019900160310     C                   READP     tntbe01l
020000160310     C                   IF        TBEKE1  = KTBEKE1
020100160310     C                   IF        TBEKE2 <= KTBEKE2 AND
020200160310     C                             TBEATB  = *blanks
020300160310     C                   SETON                                        36
020400160310     C                   ENDIF
020500160310     C                   ELSE
020600160310     C                   LEAVE
020700160310     C                   ENDIF
020800160310     C                   ENDDO
020900160310     C                   ENDIF
021000160310     C*
021100160310     C                   IF        *IN36
021200160310     C                   EVAL      pathTEST  = %trim(tbeUNI)
021300160310     C                   ENDIF
021400160310     C*
021500160310     C* Aggancio la regola BartMail indicata in input
021600160310     C                   SETOFF                                       37
021700160310     C     pIn_TIPS      CHAIN     timar01l
021800160310     C                   IF        %found(timar01l)
021900160310     C                   EVAL      pathSUBFLR = %trim(MARPOUT)
022000160311     C                   SELECT
022100160311     C                   WHEN      MARTOUT='F' AND MARTFIL='P'
022200160311     C                   EVAL      wExtFile = '.PDF'
022300160311     C                   WHEN      MARTOUT='F' AND MARTFIL='T'
022400160311     C                   EVAL      wExtFile = '.TXT'
022500160311     C                   WHEN      MARTOUT='F' AND MARTFIL='1'
022600160311     C                   EVAL      wExtFile = '.TIF'
022700160311     C                   WHEN      MARTOUT='F' AND MARTFIL='2'
022800160311     C                   EVAL      wExtFile = '.PDF'
022900160311     C                   WHEN      MARTOUT='F' AND MARTFIL='3'
023000160311     C                   EVAL      wExtFile = '.XLS'
023100160311     C                   ENDSL
023200160311     C*
023300160310     C                   SETON                                        37
023400160310     C                   ENDIF
023500160310     C*
023600160310     C* Se prerequisiti tutti validi => proseguo
023700160310     C                   IF        *in33 AND *in34 AND
023800160310     C                             *in37 AND
023900160310     C                             (
024000160310     C                             (§AOADOCFLW='P' AND *in35) OR
024100160310     C                             (§AOADOCFLW='T' AND *in36)
024200160310     C                             )
024300160310     C*
024400160310     C* Verifico se richiesta elaborazione in TEST diversamente PROD
024500160502     C                   IF        *in35 AND §AOADOCFLW ='P'
024600160310     C                   EVAL      pathOUTPUT = pathPROD
024700160310     C                   ENDIF
024800160502     C                   IF        *in36 AND §AOADOCFLW ='T'
024900160502     C                   EVAL      pathOUTPUT = pathTEST
025000160502     C                   ENDIF
025100160310     C*
025200160310     C* Quindi procedo con elaborazione
025300160310     C                   EXSR      sr_ExeElab
025400160310     C                   ELSE
025500160310     C*
025600160310     C* Altrimenti esco con errore
025700160310     C                   EVAL      pOut_Esito = '2'
025800160310     C                   SETON                                        LR
025900160310     C                   ENDIF
026000160310     C*
026100160310     C                   ENDIF
026200160310     C*
026300091223
026400160310
026500130307      /free
026600160310
026700160310       //--------------------------------------------------------------
026800160310       // Elaborazione
026900160310       //--------------------------------------------------------------
027000160310       BEGSR  sr_ExeElab;
027100160311
027200160311       // Operazioni iniziali?
027300160311       exsr sr_RoutInz;
027400160311
027500160310       // Scrittura WAS7IFS0F
027600160310       IFSINS    = DATAORA;
027700160310       IFSKEY    = %trim(JobName) + '/' + %trim(JobUser) + '/' +
027800160310                   %trim(%editc(JobNumber:'X'));
027900160310       IFSPTHS   = %trim(pathINPUT)+'/'+%trim(pathSUBFLR)+'/';
028000160311       IFSFILS   = %trim(pIn_NomeFile)+%trim(wExtFile);
028100160311       IFSPTHD   = %trim(pathOUTPUT)+'/'+%trim(pathSUBFLR)+'/';
028200160312       IFSFILD   = %trim(pIn_NomeFile)+%trim(pIn_ExtFile);
028300160312       IF %trim(IFSFILD) <> %trim(IFSFILS);
028400160312          IFSOPZ = 'MORE';
028500160312       ELSE;
028600160312          IFSOPZ = 'MOVE';
028700160312       ENDIF;
028800160601       IFSTRYTOT = 1000;
028900160310       IFSSTS    = *blanks;
029000160310       write WAS7IFS0;
029100160310
029200160310       // Se tutto bene => esito OK
029300160310       pOut_Esito = '1';
029400160310
029500160310       // Operazioni finali?
029600160310       exsr sr_RoutEnd;
029700160310
029800160310       ENDSR;
029900130923
030000091223
030100091223       //--------------------------------------------------------------
030200121107       // Operazioni iniziali.
030300091223       //--------------------------------------------------------------
030400091223       BEGSR  sr_RoutInz;
030500091223
030600130403         // apro il file da scrivere, se chiuso
030700130403         if not %open(WAS7IFS0F);
030800130403           open WAS7IFS0F;
030900130403         endif;
031000091223
031100091223       ENDSR;
031200121106
031300160310
031400091223       //--------------------------------------------------------------
031500121107       // Operazioni finali.
031600091223       //--------------------------------------------------------------
031700091223       BEGSR  sr_RoutEnd;
031800130109
031900160310         // Se richiesta chiusura nei parametri in input chiudo
032000160310         if pIn_TLA='C' OR pIn_TLA=*blanks;
032100160310            if %open(WAS7IFS0F);
032200160310               close WAS7IFS0F;
032300160310            endif;
032400160310            *inlr = *on;
032500160310         endif;
032600160310
032700160310         // Se KO chiudo in LR
032800160310         if pOut_Esito='2';
032900130307           *inlr = *on;
033000130307         endif;
033100130109         return;
033200091223
033300091223       ENDSR;
033400091223
033500091223      /end-free
