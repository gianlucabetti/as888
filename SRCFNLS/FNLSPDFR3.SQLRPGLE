000100970526     H*------------------------------------------------------------------------*
000200130329     H* Cruscotto di verifica stato PDF allegati
000300970526     H*------------------------------------------------------------------------*
000400130329     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500130329     h dftactgrp(*no)
000600130329     h alwnull(*inputonly)
000700130329     h bnddir('UBBNDDIR')
000800970526     F*------------------------------------------------------------------------*
000900970526     F* DATA BASE
001000970526     F*------------------------------------------------------------------------*
001100120927     Fcnaco00f  IF   E           K DISK
001200130402     Fazorg01l  IF   E           K DISK
001300130424     FfnlspdfD3 cf   e             WORKSTN INFDS(DEVDS1)
001400130402     F                                     SFILE(FNLSR2DMS:MSGULT)
001500130109     Fqsysprt   O    f  132        PRINTER usropn
001600130109     F                                     oflind(*inoa)
001700970526     D*------------------------------------------------------------------------*
001800970526     D* SCHIERE
001900970526     D*------------------------------------------------------------------------*
002000970526     D* MESSAGGI
002100970526     D*------------
002200000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* DESCRIZIONI TASTI FUNZIONALI
002500970526     D*------------
002600970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* FUNZIONI DA CARICARE A VIDEO
002900970526     D*------------
003000970526     D FUV             S             19    DIM(24)
003100970526     D*------------------------------------------------------------------------*
003200970526     D* INPUT
003300970526     D*------------------------------------------------------------------------*
003400970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003500970526     D*------------
003600970526     D FUDDS           DS
003700970526     D  FUDDE1                 1      1
003800970526     D  FUDFUN                 2      3
003900970526     D  FUDDE2                 4     19
004000970526     D  FUDFI0                20     34
004100970526     D  FUDATT                35     35
004200970526     D  FUDFI1                36     80
004300970526     D  FUDDES                 1     19
004400970526     D  FUDRIG                 1     80
004500970526     D*------------
004600970526     D* DS GESTIONE TASTI FUNZIONALI
004700970526     D*------------
004800970526     D DSKEY         E DS
004900970526     D DEVDS1          DS
005000970526     D  KEY                  369    369
005100970526     D*------------
005200970526     D* DS "XMSG" - GESTIONE MESSAGGI
005300970526     D*------------
005400970526     D DSMSG         E DS
005500970528     D*------------------
005600970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
005700970528     D*------------------
005800130416     D WLBDA8          DS                  INZ
005900970528     D  G08DAT                 1      8  0
006000970528     D  G08INV                 9     16  0
006100970528     D  G08ERR                17     17
006200970528     D  G08TGI                18     22  0
006300970526     D*------------
006400970526     D* ARCHITETTURA
006500970526     D*------------
006600970526     D KPJBA         E DS
006700130329     D*-------------------
006800130329     D* maschera campo AR8UNI
006900130329     D*-------------------
007000130329     d wAR8UNI         s            502
007100130329     D DAR8§PDF      E DS
007200020923     D*-------------------
007300020923     D* DS REPERIMENTO DATI UTENTE
007400020923     D*-------------------
007500020923     D TIBS34DS      E DS
007600020923     D DDATIUTE      E DS
007700020923     D AZUTEDS       E DS                  extname(AZUTE00F)
007800120927     D* DS REPERIMENTO PADRE/FIGLI
007900120927     D TIBS10DS      E DS
008000120928     D*-------------------
008100130424     D Dstnsd          DS
008200130424     D  §cod                          3
008300130424     D  §tip                          1
008400130424     D  §des                         25
008500130424     D  §pra                          3
008600130424     D*-------------------
008700120928
008800121003     d Status         sds
008900121003     d  SDSpgm           *proc
009000121003     d  SDSprm           *parms
009100121003     d  SDSuser              254    263
009200121003
009300130116     d ora             s               t
009400130329
009500130402     D* ricerca clienti con XCLIR
009600130402     d xCLIrds       e ds
009700130402     d ksa             s              4    dim(30)
009800130402     d ksc             s              7  0 dim(30)
009900130402     d                 ds
010000130402     d dsksn                   1      4p 0
010100130402     d dsksa                   1      4
010200130402
010300130329     D LibAR8          s             10A   inz
010400130329     D LibBLP          s             10A   inz
010500130329     D currSysNeta     s              8A   inz('*NULL')
010600130329     D wSQL            s           5000A
010700130329     d pIn_DA          s              8s 0
010800130329     d pIn_A           s              8s 0
010900130329
011000130402     d Digits          c                   const('0123456789')
011100130402
011200130329     D*------------------
011300130329     D* LINKING A DEFINIZIONI ESTERNE
011400130329     D*------------------
011500130329     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
011600130329     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
011700130416
011800130416
011900130416     D PDF100R         PR
012000130416     D                                     extpgm('PDF100R')
012100130416     D iTBKEY                              like(TBKEY)
012200130416     D iTBELE                              like(TBELE)
012300130417     D iPGM                          10A
012400130416     D iTBDAT                              like(TBDAT)
012500130416     D iEsito                         1A
012600130416
012700130416
012800130418     D FNLSPDFC3       PR
012900130418     D                                     extpgm('FNLSPDFC3')
013000130416     D i2PathOrig                   120A
013100130416     D i2PathCli                           like(§AR8PPDFP)
013200130416     D i2NomePDF                           like(§AR8PPDFN)
013300130416     D iCheckOnly                     1A
013400130416     D i2FileName                   256A
013500130416     D i2Esito                        1A
013600130416
013700130416
013800130416     D pdftabel      e ds
013900130416     D pPathOrig       s            120A
014000130416     D pPathCli        s                   like(§AR8PPDFP)
014100130416     D pNomePDF        s                   like(§AR8PPDFN)
014200130416     D pCheckOnly      s              1A
014300130416     D pFileName       s            256A
014400130416     D pEsito          s              1A
014500130416     D pTBKEY          s                   like(TBKEY)
014600130416     D pTBELE          s                   like(TBELE)
014700130417     D pPGM            s             10A
014800130416     D pTBDAT          s                   like(TBDAT)
014900130416
015000130416
015100130402     D* Ricerca cliente
015200130402     d*xclir           pr                  extpgm('XCLIR')
015300130402     d* xclirds                            likeds(XCLIRDS)
015400120928
015500970526     C*------------------------------------------------------------------------*
015600000303     C* MAIN LINES
015700970526     C*------------------------------------------------------------------------*
015800970526     C*
015900120927     C* metto i default su D03
016000120927     C                   EXSR      InzD03
016100120927     C*
016200120927     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
016300991214     C     F03L00        DOUEQ     '1'
016400970526     C     F12L00        OREQ      '1'
016500970526     C     FINL00        OREQ      '1'
016600970526     C*
016700970526     C* EMETTE FORMATO VIDEO
016800970526     C                   EXSR      BUTFMR
016900970526     C*
017000970526     C* GESTIONE TASTI FUNZIONALI
017100970526     C                   EXSR      FUNCON
017200970526E   2C                   ENDDO
017300970526     C*
017400970526     C* FINE PGM
017500970526     C                   SETON                                        LR
017600970526     C*------------------------------------------------------------------------*
017700970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
017800970526     C*------------------------------------------------------------------------*
017900970526     C     BUTFMR        BEGSR
018000970526     C*
018100970526     C* EMETTE LA TESTATA
018200130329     C                   WRITE     FNLSR2D01
018300970526     C*
018400970526     C* SPEDISCE EVENTUALE MESSAGGIO
018500970526     C                   EXSR      SNDMSG
018600970526     C*
018700970526     C* EMETTE LE FUNZIONI
018800130329     C                   WRITE     FNLSR2D02
018900970526     C*
019000970526     C* EMETTE IL VIDEO
019100130329     C                   EXFMT     FNLSR2D03
019200970526     C*
019300991214     C* REIMPOSTA FLAG DI ERRORE
019400991214     C                   MOVEL     '0'           ERRL00
019500970526     C*
019600970526     C* REIMPOSTA INDICATORI DI ERRORE
019700020611     C                   SETOFF                                       555556
019800970526     C*
019900970526     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100120927     C* InzD03 - Inizializzo D03
020200970526     C*------------------------------------------------------------------------*
020300120927     C     InzD03        BEGSR
020400120927     C*
020500130424     C                   EVAL      VIDRicv = 'N'
020600130108     C*
020700120927     C                   ENDSR
020800120927     C*------------------------------------------------------------------------*
020900120927     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
021000120927     C*------------------------------------------------------------------------*
021100120927     C     SNDMSG        BEGSR
021200120927     C*
021300970526     C* PULISCE IL SFILE MESSAGGI
021400970526     C                   MOVEL     '1'           *IN40
021500130329     C                   WRITE     FNLSR2DMC
021600970526     C                   MOVEL     '0'           *IN40
021700970526     C*
021800970526     C* VISUALIZZA IL MESSAGGIO
021900970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
022000970526     C                   CALL      'XMSG'        XMSGPA
022100020611     C     DSMSER        IFEQ      *ZEROS
022200970526     C                   MOVEL     DSMSKE        MSGKEY
022300020611     C                   ENDIF
022400970526     C                   Z-ADD     1             MSGULT
022500130329     C                   WRITE     FNLSR2DMS
022600130329     C                   WRITE     FNLSR2DMC
022700970526     C*
022800970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
022900970526     C                   MOVEL     *BLANKS       DSMSMS
023000970526     C*
023100970526     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300970526     C* ENTGES - GESTIONE ENTER -> INVIO
023400970526     C*------------------------------------------------------------------------*
023500970526     C     ENTGES        BEGSR
023600970526     C*
023700970526     C* CONTROLLA I CAMPI IMMESSI
023800970526     C                   EXSR      DECODI
023900970526     C*
024000970526     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200970526     C* F03GES - GESTIONE F03 -> FINE
024300970526     C*------------------------------------------------------------------------*
024400970526     C     F03GES        BEGSR
024500970526     C*
024600970526     C* IMPOSTA "PREMUTO F03"
024700970526     C                   MOVEL     '1'           F03L00
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100991214     C* F06GES - GESTIONE F06 -> CONFERMA
025200970526     C*------------------------------------------------------------------------*
025300970526     C     F06GES        BEGSR
025400970526     C*
025500991214     C* CONTROLLA I CAMPI IMMESSI
025600991214     C                   EXSR      DECODI
025700020611     C*
025800991214     C* ESEGUE ROUTINE DI CONFERMA
025900020611     C     ERRL00        IFEQ      '0'
026000991214     C                   EXSR      ESEGUI
026100130110     C* il messaggio di simulazione conclusa lo do sempre, a prescindere dagli e
026200121109     C                   MOVEL     MSG(20)       DSMSMS
026300121109     C* non esco mai dal pgm
026400121109     C***                MOVEL     '1'           FINL00
026500121108     C                   ENDIF
026600970526     C*
026700991214     C                   ENDSR
026800970526     C*------------------------------------------------------------------------*
026900130109     C* F07GES - GESTIONE F07 -> STAMPA
027000970526     C*------------------------------------------------------------------------*
027100130109     C     F07GES        BEGSR
027200970526     C*
027300130110     C* eseguo la conferma
027400130110     C                   EXSR      F06Ges
027500130329     C* la stampa la faccio solo se non ci sono errori
027600130110     C     ERRL00        IFEQ      '0'
027700130110     C*
027800130109     C                   if        not %open(qsysprt)
027900130109     C                   open      qsysprt
028000130116     C                   eval      ora = %time()
028100130109     C                   except    testdett
028200130109     C                   endif
028300130109     C*
028400130109     C                   if        %open(qsysprt)
028500130109     C                   except    findett
028600130109     C                   endif
028700130109     C*
028800130109     C                   if        %open(qsysprt)
028900130109     C                   close     qsysprt
029000130109     C                   endif
029100970526     C*
029200130110     C                   MOVEL     MSG(22)       DSMSMS
029300130110     C                   ENDIF
029400130110     C*
029500970526     C                   ENDSR
029600130109     C*------------------------------------------------------------------------*
029700130109     C* F12GES - GESTIONE F12 -> RITORNO
029800130109     C*------------------------------------------------------------------------*
029900130109     C     F12GES        BEGSR
030000130109     C*
030100130109     C* IMPOSTA "PREMUTO F12"
030200130109     C                   MOVEL     '1'           F12L00
030300130109     C*
030400130109     C                   ENDSR
030500970526     C*------------------------------------------------------------------------*
030600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
030700970526     C*------------------------------------------------------------------------*
030800970526     C     F24GES        BEGSR
030900970526     C*
031000970526     C                   EXSR      FUNGES
031100970526     C*
031200970526     C                   ENDSR
031300970526     C*------------------------------------------------------------------------*
031400991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
031500970526     C*------------------------------------------------------------------------*
031600991214     C     ESEGUI        BEGSR
031700130329     C*
031800130416     c                   EVAL      VidRicvS = 0
031900130408     c                   EVAL      VidTratS = 0
032000130408     c                   EVAL      VidStamS = 0
032100130408     c                   EVAL      VidErroS = 0
032200130408     c                   EVAL      VidAtteS = 0
032300130408     c                   EVAL      VidTOTALES = 0
032400130408     C*
032500130416     c                   EVAL      VidRicvE = 0
032600130408     c                   EVAL      VidTratE = 0
032700130408     c                   EVAL      VidErroE = 0
032800130408     c                   EVAL      VidAtteE = 0
032900130408     c                   EVAL      VidTOTALEE = 0
033000121108     C*
033100130329     C* compongo SQL
033200130329     C                   EXSR      StringSQL
033300130329     C*
033400130329
033500130329     c/exec sql
033600130329     c+ set option dynusrprf = *owner, closqlcsr = *endmod
033700130329     c/end-exec
033800130329
033900130329     c*          preparazione del cursore
034000130329     C/EXEC SQL
034100130329     C+ Prepare S1 from :wSQL
034200130329     C/END-EXEC
034300130329     c*          dichiarazione del cursore
034400130329     C/EXEC SQL
034500130329     C+ Declare A1 Cursor for S1
034600130329     C/END-EXEC
034700130329      *          apertura cursore
034800130329     C/EXEC SQL
034900130329     C+ OPEN A1
035000130329     C/END-EXEC
035100130329     C                   DOU       SqlCod < 0 OR
035200130329     C                             SqlCod = 100
035300130329      *          lettura cursore
035400130329     C/EXEC SQL
035500130329     C+ Fetch Next From A1 Into :wAR8UNI
035600130329     C/END-EXEC
035700130329     C                   SELECT
035800130329     C                   WHEN      SqlCod = 100
035900130329     c                   leave
036000130329      *
036100130329     C                   WHEN      SqlCod < 0
036200130329     c                   leave
036300130329      *
036400130329     C                   OTHER
036500130329      *
036600130329     C                   MOVEL     wAR8Uni       DAR8§PDF
036700130416      * conteggio i PDF ricevuti
036800130424      * se richiesto
036900130424     C                   IF        VIDRicv = 'S'
037000130416      /free
037100130416
037200130416            pPathCli   = §AR8PPDFP;
037300130416            pNomePDF   = §AR8PPDFN;
037400130416            pCheckOnly = 'Y';
037500130416
037600130418            callP(e) FNLSPDFC3 (pPathOrig  :
037700130416                                pPathCli   :
037800130416                                pNomePDF   :
037900130416                                pCheckOnly :
038000130416                                pFileName  :
038100130416                                pEsito);
038200130416
038300130416            // Verifico esito
038400130416            if %error or pEsito <> '0';
038500130416            else;
038600130416               VidRicvS = VidRicvS + 1;
038700130416               VidRicvE = VidRicvE + 1;
038800130416            endif;
038900130416
039000130416      /end-free
039100130424     C                   ENDIF
039200130416      *
039300130329      * conteggio i rcd letti
039400130416     c                   SELECT
039500130329     c                   WHEN      §AR8PTRT = 'S'
039600130408     c                   EVAL      VidTratS = VidTratS + 1
039700130405
039800130405     c                   WHEN      §AR8PTRT = 'X'
039900130408     c                   EVAL      VidStamS = VidStamS + 1
040000130329
040100130329     c                   WHEN      §AR8PTRT = 'E'
040200130408     c                   EVAL      VidErroS = VidErroS + 1
040300130329
040400130329     c                   WHEN      §AR8PTRT = *blank
040500130408     c                   EVAL      VidAtteS = VidAtteS + 1
040600130329
040700130405     C                   ENDSL
040800130405
040900130405
041000130405     c                   SELECT
041100130408     c                   WHEN      §AR8PEML = 'S'
041200130408     c                   EVAL      VidTratE = VidTratE + 1
041300130408
041400130408     c                   WHEN      §AR8PEML = 'E'
041500130408     c                   EVAL      VidErroE = VidErroE + 1
041600130408
041700130408     c                   WHEN      §AR8PEML = *blank
041800130408     c                   EVAL      VidAtteE = VidAtteE + 1
041900130329      *
042000130329     C                   ENDSL
042100130402      * TOTALE
042200130408     c                   EVAL      VidTotaleS = VidTratS + VidErroS + VidAtteS
042300130408     c                                        + VidStamS
042400130408     c                   EVAL      VidTotaleE = VidTratE + VidErroE + VidAtteE
042500130329      *
042600130329     C                   ENDSL
042700130329      *
042800130329     C                   ENDDO
042900130329
043000130329     C*                  chiusura del cursore
043100130329     C/EXEC SQL
043200130329     C+ Close A1
043300130329     C/END-EXEC
043400991214     C*
043500970526     C                   ENDSR
043600970526     C*------------------------------------------------------------------------*
043700130329     C* Compongo stringa SQL
043800970526     C*------------------------------------------------------------------------*
043900130329     C     StringSQL     BEGSR
044000121019     C*
044100130329      /free
044200130329        wSQL = 'SELECT AR8UNI FROM ' + %trim(LibBLP) + '/FNBLP00F +
044300130329                inner join ' + %trim(LibAR8) + '/FIAR800F +
044400130329                on +
044500130329                BLPAAS = AR8AAS and +
044600130329                BLPLNP = AR8LNP and +
044700130329                BLPNRS = AR8NRS and +
044800130329                BLPNSP = AR8NSP +
044900130329                where +
045000130402                BLPAAS = ' + %subst(%char(pin_DA) : 1 : 4) + ' and +
045100130329                BLPDIM between ' +
045200130329                %char(pIn_DA) + ' and ' +
045300130329                %char(pIn_A) + ' and +
045400130329                AR8TMD = ''§PDF'' and BLPCBO in +
045500130402                (''1'' , ''2'' , ''4'' , ''6'')';
045600130424        if VIDCCM <> *blank;
045700130424          wSQL = %trim(wSQL) +
045800130424                'and BLPKSC = ' + VidCCM;
045900130424        endif;
046000130424        if VIDLNP <> *blank;
046100130402          wSQL = %trim(wSQL) +
046200130424                'and BLPLNP = ' + VidLNP;
046300130402        endif;
046400130402      /end-free
046500130329     C*
046600130329     C                   ENDSR
046700130329     C*------------------------------------------------------------------------*
046800130329     C* DECODI - CONTROLLA CAMPI E DECODIFICA
046900130329     C*------------------------------------------------------------------------*
047000130329     C     DECODI        BEGSR
047100130329     C*
047200120927     C                   SETOFF                                       505152
047300120927     C                   SETOFF                                       535455
047400130123     C                   SETOFF                                       56
047500000306     C*
047600130424     C* Se c'è il ? nel cliente, viene eseguita la ricerca
047700130402     C                   IF        %scan('?' : VIDCCM) > 0
047800130402     C                   clear                   xCLIrds
047900130402     C***  DXCaut = OTAA1cabi;
048000130402     C                   EVAL      DXCcap = DUTkci
048100130402     C                   CALL      'XCLIR'
048200130402     C                   PARM                    xCLIrds
048300130402     C                   EVAL      ksa = DXCksc
048400130402     C                   EVAL      Dsksa = ksa(1)
048500130402     C                   EVAL      ksc(1) = dsksn
048600130402     C                   EVAL      VIDCCM = %editc(ksc(1):'X')
048700130402     C                   ENDIF
048800130424
048900130424     C* Se c'è il ? nella filiale, viene eseguita la ricerca
049000130424     C                   IF        %scan('?' : VIDLNP) > 0
049100130424     C                   clear                   dstnsd
049200130424     C                   call      'TNSD24R'
049300130424     C                   parm                    §cod
049400130424     C                   parm                    §tip
049500130424     C                   parm                    §des
049600130424     C                   parm                    §pra
049700130424     C                   movel     §cod          VIDLNP
049800130424     C                   movel     §des          VDDLNP
049900130424     C                   endif
050000130402
050100130402     C*
050200130402     C* Ctrl i campi obbligatori
050300120927     C     ERRL00        IFEQ      '0'
050400120927     C                   SELECT
050500130424     C* almeno un dato tra cod.cliente e linea di partenza
050600130424     C                   WHEN      VIDCCM = *blank and
050700130424     C                             VIDLNP = *blank
050800130402     C                   clear                   VDDCCM
050900130424     C                   clear                   VDDLNP
051000121108     C                   MOVEL     '1'           ERRL00
051100130424     C                   SETON                                        5152
051200130424     C                   MOVEL     MSG(7)        DSMSMS
051300120927     C                   ENDSL
051400120927     C                   ENDIF
051500121001     C*
051600130402     C* Cod. cliente mittente solo valori numerici
051700121001     C     ERRL00        IFEQ      '0'
051800130402     C                   IF        VIDCCM <> *blank
051900130402     C                   IF        %check(digits:VIDCCM) > 0
052000130402     C                   clear                   VDDCCM
052100130402     C                   MOVEL     '1'           ERRL00
052200130402     C                   SETON                                        51
052300130402     C                   MOVEL     MSG(3)        DSMSMS
052400130402     C                   ENDIF
052500130402     C                   ENDIF
052600130402     C                   ENDIF
052700130402     C*
052800130402     C*
052900130402     C* Cod. cliente mittente esistente su CNACO
053000130402     C     ERRL00        IFEQ      '0'
053100130402     C                   IF        VIDCCM <> *blank
053200120927     C                   Z-ADD     1             ACOkut
053300120927     C                   Z-ADD     DUTKCI        ACOkcc
053400130402     C                   EVAL      ACOKsc = %dec(VIDCCM : 7 :0)
053500120927     C     K03ACO00      CHAIN     cnaco00f
053600120927     C                   IF        %found(cnaco00f)
053700130329     C                   MOVEL     ACOrag        VDDCCM
053800120927     C                   ELSE
053900120927     C                   MOVEL     '1'           ERRL00
054000130108     C                   SETON                                        51
054100120927     C                   MOVEL     MSG(3)        DSMSMS
054200130329     C                   MOVEL     *BLANKS       VDDCCM
054300120927     C                   ENDIF
054400130123     C                   ENDIF
054500120927     C                   ENDIF
054600130108     C*
054700130108     C* Data DA formalmente valida
054800130424     C* prima controllo se la data è in formato ddMMaaaa
054900130108     C     ERRL00        IFEQ      '0'
055000130108     C     *eur          TEST(D)                 VIDDA                  50
055100130108     C                   IF        *in50 = *on
055200130424     C* se non lo è, controllo se la data è in formato ddMMaa
055300130424     C     *dmy          TEST(D)                 VIDDA                  50
055400130424     C                   IF        *in50 = *on
055500130424     C* se non è neanche in questo, errore
055600130108     C                   MOVEL     '1'           ERRL00
055700130108     C                   MOVEL     MSG(14)       DSMSMS
055800130108     C                   ELSE
055900130424     c                   EVAL      pIn_Da      = %dec(%date(VIDDa:*dmy):*iso)
056000130424     C* a video metto la data di 8 in formato ggMMaaaa
056100130424     c                   EVAL      VIDDa       = %dec(%date(PIn_Da):*eur)
056200130108     C                   ENDIF
056300130424     C                   ELSE
056400130424     c                   EVAL      pIn_Da      = %dec(%date(VIDDa:*eur):*iso)
056500130108     C                   ENDIF
056600130424     C                   ENDIF
056700130108     C*
056800130108     C* Data A  formalmente valida
056900130108     C     ERRL00        IFEQ      '0'
057000130329     C* se Data A è vuota, la pongo = Data DA
057100130329     C                   IF        VidA = 0
057200130329     C                   EVAL      VidA = VidDA
057300130329     C                   ENDIF
057400130424     C* prima controllo se la data è in formato ddMMaaaa
057500130123     C     *eur          TEST(D)                 VIDA                   56
057600130123     C                   IF        *in56 = *on
057700130424     C* se non lo è, controllo se la data è in formato ddMMaa
057800130424     C     *dmy          TEST(D)                 VIDA                   56
057900130424     C                   IF        *in56 = *on
058000130424     C* se non è neanche in questo, errore
058100130108     C                   MOVEL     '1'           ERRL00
058200130108     C                   MOVEL     MSG(14)       DSMSMS
058300130424     C                   ELSE
058400130424     c                   EVAL      pIn_A       = %dec(%date(VIDA:*dmy):*iso)
058500130424     C* a video metto la data di 8 in formato ggMMaaaa
058600130424     c                   EVAL      VIDA        = %dec(%date(PIn_A):*eur)
058700130424     C                   ENDIF
058800130108     C                   ELSE
058900130108     c                   EVAL      pIn_A       = %dec(%date(VIDA:*eur):*iso)
059000130108     C                   ENDIF
059100130108     C                   ENDIF
059200130108     C*
059300130108     C* Data A >= Data Da
059400130108     C     ERRL00        IFEQ      '0'
059500130108     C                   IF        pIn_A < pIn_Da
059600130123     C                   SETON                                        5056
059700130108     C                   MOVEL     '1'           ERRL00
059800130108     C                   MOVEL     MSG(15)       DSMSMS
059900130108     C                   ENDIF
060000130108     C                   ENDIF
060100130402     C*
060200130402     C* il periodo di estrazione deve essere su stesso anno
060300130402     C     ERRL00        IFEQ      '0'
060400130402     C                   IF        %subst(%char(pIn_A) : 1 : 4) <>
060500130402     C                             %subst(%char(pIn_DA) : 1 : 4)
060600130402     C                   SETON                                        5056
060700130402     C                   MOVEL     '1'           ERRL00
060800130402     C                   MOVEL     MSG(16)       DSMSMS
060900130402     C                   ENDIF
061000130402     C                   ENDIF
061100130424     C*
061200130424     C* filiale solo valori numerici
061300130424     C     ERRL00        IFEQ      '0'
061400130424     C* se la filiale è 000, l'abblenco
061500130424     C                   IF        VIDLNP = '000'
061600130424     C                   EVAL      VIDLNP = *blank
061700130424     C                   ENDIF
061800130424     C                   IF        VIDLNP <> *blank
061900130424     C                   IF        %check(digits:VIDLNP) > 0
062000130424     C                   clear                   VDDLNP
062100130424     C                   MOVEL     '1'           ERRL00
062200130424     C                   SETON                                        52
062300130424     C                   MOVEL     MSG(4)        DSMSMS
062400130424     C                   ENDIF
062500130424     C                   ENDIF
062600130424     C                   ENDIF
062700130402     C*
062800130402     C* la filiale di partenza deve esistere
062900130402     C     ERRL00        IFEQ      '0'
063000130424     C                   IF        VidLNP <> *blank
063100130424     C                   EVAL      ORGFIL = %dec(VIDLNP : 3 : 0)
063200130424     C     ORGFIL        CHAIN     AZORG                              28
063300130402     C                   IF        *in28 = *on  OR
063400130402     C                             ORGFAG <> 'A'  AND
063500130402     C                             ORGFAG <> 'F'
063600130402     C                   SETON                                        52
063700130424     C                   clear                   VDDLNP
063800130402     C                   MOVEL     '1'           ERRL00
063900130402     C                   MOVEL     MSG(17)       DSMSMS
064000130424     C                   ELSE
064100130424     C                   EVAL      VDDLNP = ORGDES
064200130424     C                   ENDIF
064300130402     C                   ENDIF
064400130402     C                   ENDIF
064500120927     C*
064600970528     C                   ENDSR
064700970526     C*------------------------------------------------------------------------*
064800970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
064900970526     C*------------------------------------------------------------------------*
065000970526     C     AZZVID        BEGSR
065100970526     C* CAMPI
065200970526     C*
065300970526     C                   ENDSR
065400970526     C*------------------------------------------------------------------------*
065500020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
065600970526     C*------------------------------------------------------------------------*
065700970526     C     IMPFUN        BEGSR
065800970526     C*
065900970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
066000970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
066100970526     C                   EXSR      FUNGES
066200970526     C*
066300970526     C                   ENDSR
066400020923     C*-------------------------------------------------------------------------
066500020923     C* REPERISCE I DATI UTENTE
066600020923     C*-------------------------------------------------------------------------
066700020923     C     REPDATIUTE    BEGSR
066800020923     C*
066900020923     C* INIZIALIZZA VARIABILI DI WRK
067000020923     C                   CLEAR                   TIBS34DS
067100020923     C                   CLEAR                   AZUTEDS
067200020923     C                   CLEAR                   DDATIUTE
067300020923     C*
067400020923     C     *DTAARA       DEFINE    §azute        azuteds
067500020923     C     *DTAARA       DEFINE    §datiute      ddatiute
067600020923     C                   IN(E)     *DTAARA
067700020923if  1C                   IF        %Error
067800020923     c                   EVAL      I34Tla = 'L'
067900020923     C                   CALL      'TIBS34R'
068000020923     C                   PARM                    Tibs34Ds
068100020923     C                   IN        *DTAARA
068200020923e   1C                   ENDIF
068300020923     C*
068400020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
068500020923     C                   MOVEL(P)  RSUT          DSFIRS           20
068600020923     C*
068700020923     C                   ENDSR
068800970526     C*------------------------------------------------------------------------*
068900970526     C* *INZSR - OPERAZIONI INIZIALI
069000970526     C*------------------------------------------------------------------------*
069100970526     C     *INZSR        BEGSR
069200991217     C*
069300991217     C     *ENTRY        PLIST
069400991217     C                   PARM                    KPJBA
069500120927     C*
069600120927     C* LETTURA cnaco00f -
069700120927     C     K03ACO00      KLIST
069800120927     C                   KFLD                    ACOkut
069900120927     C                   KFLD                    ACOkcc
070000120927     C                   KFLD                    ACOksc
070100130329     C*
070200130329     C* Reperisco il sistema AS/400 corrente
070300130329     C* e, a seconda di qual è, imposto la libreria corretta per ogni file
070400130329     C                   callp     UBRTVNETA_Rtv(currSysNeta)
070500130329     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
070600130329     C                   eval      LibAR8 = 'FILTRA201'
070700130329     C                   eval      LibBLP = 'FILTRA201'
070800130329     C                   else
070900130329     C                   eval      LibAR8 = 'FILTRAPRD'
071000130329     C                   eval      LibBLP = 'FILTRAPRD'
071100130329     C                   endif
071200130416     C*
071300130416      /free
071400130416
071500130416         // Reperisco i parametri di procedura da PDFTABEL
071600130416         pTBKEY = 'BRT';
071700130416         pTBELE = 'PATHORIGINE';
071800130417         pPGM   = 'FNLSPDFR2';
071900130416         pTBDAT = *blanks;
072000130416         callP(e) PDF100R (pTBKEY :
072100130416                           pTBELE :
072200130417                           pPGM   :
072300130416                           pTBDAT :
072400130416                           pEsito);
072500130416         if %error or pEsito <> '0';
072600130416         else;
072700130416            pPathOrig = %trim(pTBDAT);
072800130416         endif;
072900130416
073000130416      /end-free
073100130416      *
073200130416     C*
073300020923     C*
073400020923     C* REPERISCE I DATI DELL'UTENTE
073500020923     C                   EXSR      REPDATIUTE
073600970526     C*
073700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
073800970526     C     XMSGPA        PLIST
073900970526     C                   PARM                    DSMSMS
074000970526     C                   PARM                    DSMSPG
074100970526     C                   PARM                    DSMSKE
074200970526     C                   PARM                    DSMSER
074300970526     C*
074400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
074500970526     C                   CALL      'XKEY'
074600970526     C                   PARM                    DSKEY
074700970526     C*---
074800970526     C* VARIABILI NON RIFERITE AL DATA BASE
074900970526     C*---
075000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
075100991214     C                   MOVEL     *BLANKS       DEPOP1            3
075200970526     C*---
075300970526     C* VARIABILI DI CONTROLLO
075400970526     C*---
075500970526     C                   MOVEL     '0'           FINL00            1
075600970526     C                   MOVEL     '0'           F03L00            1
075700970526     C                   MOVEL     '0'           F12L00            1
075800970526     C                   MOVEL     '0'           ERRL00            1
075900970526     C*
076000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
076100130329     C                   MOVEL     'FNLSPDFR2'   DSMSPG
076200130329     C                   MOVEL     'FNLSPDFR2'   MSGQUE
076300970526     C                   MOVEL     *BLANKS       MSGKEY
076400970526     C                   MOVEL     *BLANKS       DSMSMS
076500970528     C*
076600970528     C* CALCOLA LA DATA CORRENTE
076700970528     C                   TIME                    WN14             14 0
076800970528     C                   MOVE      WN14          WN8               8 0
076900970528     C                   Z-ADD     WN8           G08DAT
077000970528     C                   Z-ADD     *ZEROS        G08INV
077100970528     C                   MOVEL     '0'           G08ERR
077200970528     C                   CALL      'XSRDA8'
077300970528     C                   PARM                    WLBDA8
077400970528     C                   Z-ADD     G08INV        DATCOR            8 0
077500000310     C*
077600000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
077700000310     C                   EXSR      AZZVID
077800970526     C*
077900970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
078000970526     C                   EXSR      IMPFUN
078100970526     C*
078200970526     C                   ENDSR
078300970526     C*------------------------------------------------------------------------*
078400970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
078500970526     C*------------------------------------------------------------------------*
078600970526     C     FUNGES        BEGSR
078700970526     C*
078800970526     C* PULISCO CAMPI DI LAVORO
078900970526     C                   Z-ADD     0             W                 5 0
079000970526     C*
079100970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
079200970526     C* E CARICO TABELLA FUNZIONI ATTIVE
079300970526     C     1             DO        24            I
079400970526     C                   MOVEL     FUD(I)        FUDRIG
079500970526     C*
079600970526     C* PULIZIA FUNZIONI PRECEDENTI
079700970526     C                   MOVEL     *BLANKS       FUV(I)
079800970526     C*
079900970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
080000970526     C* (SALTA F24)
080100970526     C     FUDATT        IFEQ      'S'
080200970526     C     FUDFUN        ANDNE     '24'
080300970526     C                   ADD       1             W
080400970526     C                   MOVEL     FUDDES        FUV(W)
080500970526     C                   END
080600970526     C                   END
080700970526     C*
080800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
080900970526     C* ALTRIMENTI DISATTIVO F24
081000970526     C     1             DO        24            I
081100970526     C                   MOVEL     FUD(I)        FUDRIG
081200970526     C*
081300970526     C     FUDFUN        IFEQ      '24'
081400970526     C     W             IFGT      6
081500970526     C                   MOVEL     'S'           FUDATT
081600970526     C                   ELSE
081700970526     C                   MOVEL     *BLANKS       FUDATT
081800970526     C                   END
081900970526     C                   MOVEL     FUDRIG        FUD(I)
082000970526     C                   END
082100970526     C                   END
082200970526     C*
082300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
082400970526     C     W             DIV       6             I
082500970526     C                   MVR                     Z
082600970526     C*
082700970526     C     Z             IFNE      0
082800970526     C     I             ADD       1             Z
082900970526     C                   ELSE
083000970526     C                   Z-ADD     I             Z                 5 0
083100970526     C                   END
083200970526     C*
083300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
083400970526     C                   ADD       1             FUNGRU
083500970526     C     FUNGRU        IFGT      Z
083600970526     C     FUNGRU        ORLT      1
083700970526     C                   Z-ADD     1             FUNGRU
083800970526     C                   END
083900970526     C*
084000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
084100970526     C     FUNGRU        IFEQ      1
084200970526     C                   MOVEL     FUV(1)        FUN001
084300970526     C                   MOVEL     FUV(2)        FUN002
084400970526     C                   MOVEL     FUV(3)        FUN003
084500970526     C                   MOVEL     FUV(4)        FUN004
084600970526     C                   MOVEL     FUV(5)        FUN005
084700970526     C                   MOVEL     FUV(6)        FUN006
084800970526     C                   END
084900970526     C*
085000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
085100970526     C     FUNGRU        IFEQ      2
085200970526     C                   MOVEL     FUV(7)        FUN001
085300970526     C                   MOVEL     FUV(8)        FUN002
085400970526     C                   MOVEL     FUV(9)        FUN003
085500970526     C                   MOVEL     FUV(10)       FUN004
085600970526     C                   MOVEL     FUV(11)       FUN005
085700970526     C                   MOVEL     FUV(12)       FUN006
085800970526     C                   END
085900970526     C*
086000970526     C* TERZO GRUPPO DI 6 FUNZIONI
086100970526     C     FUNGRU        IFEQ      3
086200970526     C                   MOVEL     FUV(13)       FUN001
086300970526     C                   MOVEL     FUV(14)       FUN002
086400970526     C                   MOVEL     FUV(15)       FUN003
086500970526     C                   MOVEL     FUV(16)       FUN004
086600970526     C                   MOVEL     FUV(17)       FUN005
086700970526     C                   MOVEL     FUV(18)       FUN006
086800970526     C                   END
086900970526     C*
087000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
087100970526     C     FUNGRU        IFEQ      4
087200970526     C                   MOVEL     FUV(19)       FUN001
087300970526     C                   MOVEL     FUV(20)       FUN002
087400970526     C                   MOVEL     FUV(21)       FUN003
087500970526     C                   MOVEL     FUV(22)       FUN004
087600970526     C                   MOVEL     *BLANKS       FUN005
087700970526     C                   MOVEL     *BLANKS       FUN006
087800970526     C                   END
087900970526     C*
088000970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
088100970526     C                   MOVEL     *BLANKS       FUN007
088200970526     C                   MOVEL     *BLANKS       FUN008
088300970526     C*
088400970526     C     1             DO        24            I
088500970526     C                   MOVEL     FUD(I)        FUDRIG
088600970526     C     FUDFUN        IFEQ      '24'
088700970526     C     FUDATT        ANDEQ     'S'
088800970526     C                   MOVEL     FUDDES        FUN008
088900970526     C                   END
089000970526     C                   END
089100970526     C*
089200970526     C                   ENDSR
089300970526     C*------------------------------------------------------------------------*
089400970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
089500970526     C*------------------------------------------------------------------------*
089600970526     C     FUNCON        BEGSR
089700970526     C*
089800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
089900970526     C                   EXSR      FUNVAR
090000970526     C*
090100970526     C* PULISCO CAMPI DI LAVORO
090200970526     C                   Z-ADD     0             I                 5 0
090300970526     C                   Z-ADD     0             W                 5 0
090400970526     C*
090500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
090600970526     C     FUNKEY        IFNE      *BLANKS
090700970526     C     1             DO        24            I
090800970526     C                   MOVEL     FUD(I)        FUDRIG
090900970526     C*
091000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
091100970526     C     FUNKEY        IFEQ      FUDFUN
091200970526     C     FUDATT        ANDEQ     'S'
091300970526     C                   Z-ADD     I             W
091400970526     C                   END
091500970526     C                   END
091600970526     C*
091700970526     C* FUNZIONE NON ATTIVA (ERRORE)
091800970526     C     W             IFEQ      0
091900970526     C*
092000970526     C* EMISSIONE MESSAGGIO DI ERRORE
092100970526     C                   MOVEL     MSG(1)        DSMSMS
092200970526     C*
092300970526     C                   END
092400970526     C                   END
092500970526     C*
092600970526     C* FUNZIONE ATTIVA (PROCEDI ...)
092700970526     C     W             IFNE      0
092800970526     C     FUNKEY        OREQ      *BLANKS
092900970526     C     FUNKEY        IFEQ      *BLANKS
093000970526     C                   EXSR      ENTGES
093100970526     C                   END
093200970526     C     FUNKEY        IFEQ      '03'
093300970526     C                   EXSR      F03GES
093400970526     C                   END
093500970526     C     FUNKEY        IFEQ      '06'
093600970526     C                   EXSR      F06GES
093700970526     C                   END
093800130109     C     FUNKEY        IFEQ      '07'
093900130109     C                   EXSR      F07GES
094000130109     C                   END
094100970526     C     FUNKEY        IFEQ      '12'
094200970526     C                   EXSR      F12GES
094300970526     C                   END
094400970526     C     FUNKEY        IFEQ      '24'
094500970526     C                   EXSR      F24GES
094600970526     C                   END
094700970526     C                   END
094800970526     C*
094900970526     C                   ENDSR
095000970526     C*------------------------------------------------------------------------*
095100970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
095200970526     C*------------------------------------------------------------------------*
095300970526     C     FUNVAR        BEGSR
095400970526     C*
095500970526     C* INIZIALIZZA VARIABILE -FUNKEY-
095600970526     C*
095700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
095800970526     C     KEY           IFEQ      ENTER
095900970526     C                   MOVEL     *BLANKS       FUNKEY
096000970526     C                   END
096100970526     C     KEY           IFEQ      F1
096200970526     C                   MOVEL     '01'          FUNKEY
096300970526     C                   END
096400970526     C     KEY           IFEQ      F2
096500970526     C                   MOVEL     '02'          FUNKEY
096600970526     C                   END
096700970526     C     KEY           IFEQ      F3
096800970526     C                   MOVEL     '03'          FUNKEY
096900970526     C                   END
097000970526     C     KEY           IFEQ      F4
097100970526     C                   MOVEL     '04'          FUNKEY
097200970526     C                   END
097300970526     C     KEY           IFEQ      F5
097400970526     C                   MOVEL     '05'          FUNKEY
097500970526     C                   END
097600970526     C     KEY           IFEQ      F6
097700970526     C                   MOVEL     '06'          FUNKEY
097800970526     C                   END
097900970526     C     KEY           IFEQ      F7
098000970526     C                   MOVEL     '07'          FUNKEY
098100970526     C                   END
098200970526     C     KEY           IFEQ      F8
098300970526     C                   MOVEL     '08'          FUNKEY
098400970526     C                   END
098500970526     C     KEY           IFEQ      F9
098600970526     C                   MOVEL     '09'          FUNKEY
098700970526     C                   END
098800970526     C     KEY           IFEQ      F10
098900970526     C                   MOVEL     '10'          FUNKEY
099000970526     C                   END
099100970526     C     KEY           IFEQ      F11
099200970526     C                   MOVEL     '11'          FUNKEY
099300970526     C                   END
099400970526     C     KEY           IFEQ      F12
099500970526     C                   MOVEL     '12'          FUNKEY
099600970526     C                   END
099700970526     C     KEY           IFEQ      F13
099800970526     C                   MOVEL     '13'          FUNKEY
099900970526     C                   END
100000970526     C     KEY           IFEQ      F14
100100970526     C                   MOVEL     '14'          FUNKEY
100200970526     C                   END
100300970526     C     KEY           IFEQ      F15
100400970526     C                   MOVEL     '15'          FUNKEY
100500970526     C                   END
100600970526     C     KEY           IFEQ      F16
100700970526     C                   MOVEL     '16'          FUNKEY
100800970526     C                   END
100900970526     C     KEY           IFEQ      F17
101000970526     C                   MOVEL     '17'          FUNKEY
101100970526     C                   END
101200970526     C     KEY           IFEQ      F18
101300970526     C                   MOVEL     '18'          FUNKEY
101400970526     C                   END
101500970526     C     KEY           IFEQ      F19
101600970526     C                   MOVEL     '19'          FUNKEY
101700970526     C                   END
101800970526     C     KEY           IFEQ      F20
101900970526     C                   MOVEL     '20'          FUNKEY
102000970526     C                   END
102100970526     C     KEY           IFEQ      F21
102200970526     C                   MOVEL     '21'          FUNKEY
102300970526     C                   END
102400970526     C     KEY           IFEQ      F22
102500970526     C                   MOVEL     '22'          FUNKEY
102600970526     C                   END
102700970526     C     KEY           IFEQ      F23
102800970526     C                   MOVEL     '23'          FUNKEY
102900970526     C                   END
103000970526     C     KEY           IFEQ      F24
103100970526     C                   MOVEL     '24'          FUNKEY
103200970526     C                   END
103300970526     C     KEY           IFEQ      ROLLUP
103400970526     C                   MOVEL     'RU'          FUNKEY
103500970526     C                   END
103600970526     C     KEY           IFEQ      HELP
103700970526     C                   MOVEL     'HP'          FUNKEY
103800970526     C                   END
103900970526     C                   ENDSR
104000970526     C*------------------------------------------------------------------------*
104100970526     C* NOTE COSTRUZIONE SCHIERE
104200970526     C*------------------------------------------------------------------------*
104300970526     C*
104400970526     C* -MSG-MESSAGGI
104500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
104600970526     C* (NON TOCCARE MSG,1 E MSG,2)
104700970526     C*
104800970526     C* -CMD-COMANDI
104900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
105000970526     C*
105100970526     C* -OPD-OPZIONI
105200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
105300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
105400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
105500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
105600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
105700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
105800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
105900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
106000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
106100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
106200970526     C*
106300970526     C* -FUD-FUNZIONI
106400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
106500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
106600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
106700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
106800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
106900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
107000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
107100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
107200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
107300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
107400970526     O*------------------------------------------------------------------------*
107500130110     Oqsysprt   E            testdett          2
107600130116     O                       KNSIF
107700130116     O                       SDSUSER             +2
107800130329     O                                           72 '* Cruscotto di verifica'
107900130329     O                                           +1 'stati PDF *'
108000130110     O                       G08DAT        Y    130
108100130116     O          E            testdett    1
108200130116     O                       SDSPGM
108300130116     O                       ORA                130
108400130116     O          E            testdett    4
108500130329     O                                              'Cliente'
108600130329     O                       VIDCCM              +1
108700130329     O                       VDDCCM              +2
108800130123     O                                              'Periodo'
108900130110     O                       VIDDA         Y     +1
109000130110     O                                           +1 '-'
109100130110     O                       VIDA          Y     +1
109200130329     O          E            findett     3
109300130408     O                                              'STAMPA PDF'
109400130416     O          E            findett     3
109500130416     O                                              'Ricevuti ............:'
109600130416     O                       VIDRICVS      1     +2
109700130408     O          E            findett     3
109800130408     O                                              'Trattati ............:'
109900130408     O                       VIDTRATS      1     +2
110000130329     O          E            findett     3
110100130408     O                                              'Stampati ............:'
110200130408     O                       VIDSTAMS      1     +2
110300130329     O          E            findett     3
110400130329     O                                              'In errore ...........:'
110500130408     O                       VIDERROS      1     +2
110600130329     O          E            findett     3
110700130329     O                                              'In attesa ...........:'
110800130408     O                       VIDATTES      1     +2
110900130402     O          E            findett     3
111000130402     O                                              'TOTALE ..............:'
111100130408     O                       VIDTOTALES    1     +2
111200130408     O          E            findett     4
111300130408     O                                              'E-MAIL PDF'
111400130416     O          E            findett     3
111500130416     O                                              'Ricevuti ............:'
111600130416     O                       VIDRICVE      1     +2
111700130408     O          E            findett     3
111800130408     O                                              'Trattati ............:'
111900130408     O                       VIDTRATE      1     +2
112000130408     O          E            findett     3
112100130408     O                                              'In errore ...........:'
112200130408     O                       VIDERROE      1     +2
112300130408     O          E            findett     3
112400130408     O                                              'In attesa ...........:'
112500130408     O                       VIDATTEE      1     +2
112600130408     O          E            findett     3
112700130408     O                                              'TOTALE ..............:'
112800130408     O                       VIDTOTALEE    1     +2
112900970526** MSG - MESSAGGI
113000120927Tasto funzionale non gestito                                                   1
113100120927Numero chiamate obbligatorio                                                   2
113200120927Codice cliente non esiste su CNACO00F                                          3
113300130424Filiale non esiste su AZORG00F                                                 4
113400130108                                                                               5
113500130108Campo obbligatorio                                                             6
113600130424Obbligatorio almeno un dato tra cliente e linea di partenza                    7
113700130108                                                                               8
113800120927                                                                               9
113900130402                                                                               0
114000120927                                                                               1
114100130402                                                                               2
114200130402                                                                               3
114300130402Data errata                                                                    4
114400130402La data inizio deve essere > o = della data fine                               5
114500130402Il periodo di estrazione deve essere su stesso anno                            6
114600130402Filiale di partenza inesistente o non valida                                   7
114700130402                                                                               8
114800130402                                                                               9
114900130402Conteggio concluso                                                             0
115000130402Scelta obbligatoria                                                            1
115100130110Eseguita la stampa                                                             2
115200130402                                                                               3
115300130402                                                                               4
115400970526** FUD - FUNZIONI
115500970526                   -        (S/N)=N                        funz.01¬
115600970526                   -        (S/N)=N                        funz.02¬
115700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
115800970526                   -        (S/N)=N                        funz.04¬
115900970526                   -        (S/N)=N                        funz.05¬
116000991214F06=Conferma       -        (S/N)=S                        funz.06¬
116100130110F07=Conf.+Stampa   -        (S/N)=S                        funz.07¬
116200970527                   -        (S/N)=N                        funz.08¬
116300970527                   -        (S/N)=N                        funz.09¬
116400970527                   -        (S/N)=N                        funz.10¬
116500970526                   -        (S/N)=N                        funz.11¬
116600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
116700970526                   -        (S/N)=N                        funz.13¬
116800970526                   -        (S/N)=N                        funz.14¬
116900970526                   -        (S/N)=N                        funz.15¬
117000970526                   -        (S/N)=N                        funz.16¬
117100970526                   -        (S/N)=N                        funz.17¬
117200970526                   -        (S/N)=N                        funz.18¬
117300970526                   -        (S/N)=N                        funz.19¬
117400970526                   -        (S/N)=N                        funz.20¬
117500970526                   -        (S/N)=N                        funz.21¬
117600970526                   -        (S/N)=N                        funz.22¬
117700970526                   -        (S/N)=N                        funz.23¬
117800970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
