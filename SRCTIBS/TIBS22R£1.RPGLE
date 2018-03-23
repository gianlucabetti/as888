000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000413
000300000413      *--------------------------------------------------------------*
000400000413      *   RICEZIONE IN PARTENZA DELLE BOLLE RIGENERATE DA SEDE       *
000500000413      *--------------------------------------------------------------*
000600000413
000700000413      *****************************************************************
000800000413      *  RIEPILOGO INDICATORI
000900000413      *****************************************************************
001000000419      * 31    - DI COMODO
001100000419      * 91    - FILE GIA' ALLOCATO
001200000413      *****************************************************************
001300000413
001400041122     FFNBLP00R  UF   E           K DISK    USROPN RENAME(FNBLP000:FNBLPR)
001500060221     FFIAR400R  UF   E           K DISK    USROPN RENAME(FIAR4000:FIAR4R)
001600030110     FFIAR500R  Uf   e           k disk    usropn rename(FIAR5000:FIAR5R)
001700000413     FFIAR600R  UF   E           K DISK    USROPN RENAME(FIAR6000:FIAR6R)
001800000413     FFIAR700R  UF   E           K DISK    USROPN RENAME(FIAR7000:FIAR7R)
001900051116     FFiar900R  UF   E           K DISK    USROPN RENAME(Fiar9000:Fiar9R)
002000000413     FFNBLT00R  UF   E           K DISK    USROPN RENAME(FNBLT000:FNBLTR)
002100041122     F***FNLBL00R  UF   E           K DISK    USROPN RENAME(FNLBL000:FNLBLR)
002200040831     F***FNEVB00R  UF   E           K DISK    USROPN RENAME(FNEVB000:FNEVBR)
002300000413      *
002400021121     FFNBLP01L  uF A E           K DISK    prefix(o)
002500060221     FFIAR401L  UF A E           K DISK
002600030110     FFIAR501L  Uf a e           k disk
002700000426     FFIAR601L  UF A E           K DISK    prefix(o)
002800991126     FFIAR701L  UF A E           K DISK
002900051116     FFiar901L  UF A E           K DISK
003000000427     FFNBLT27L  UF A E           K DISK
003100041122     F****FNLBL02L  UF A E           K DISK
003200040831     F****FNEVB01L  UF A E           K DISK
003300050909     fTigcp51l  uf   e           k disk
003400000413      *
003500041122     D SCadd           S             55    DIM(7) CTDATA PERRCD(1)
003600041122     D SCalc           S             55    DIM(7) CTDATA PERRCD(1)
003700041122     D SCovr           S             55    DIM(7) CTDATA PERRCD(1)
003800000413      *
003900000000     D KPJBA         E DS
004000900526     D  LEGFIL               483    492
004100900526     D  MBRFIL               493    502
004200000413     D UT§DSE0F      E DS
004300991126     D CNCR80        E DS
004400000426     D DS_FIAR6      E DS                  extname(FIAR600F)
004500000426     D DS_FIAR6_O    E DS                  extname(FIAR600F) prefix(o)
004600021121     D DS_Fnblp      E DS                  extname(Fnblp00F)
004700021121     D DS_Fnblp_O    E DS                  extname(Fnblp00F) prefix(o)
004800000413      *
004900000414     D XX              S              3  0
005000000414     D lung            S             15  5 INZ(55)
005100000413     D comman          S             80
005200000413     D Wnumele         S              5P 0
005300000413     D RSUT            S             20
005400000427     D Wsegna          S              1
005500000413     D W0140           S             14  0
005600000413     D UDATE8          S              8  0
005700000414     D UDATE8G         S              8  0
005800000419     d RECBLP          s              8  0   inz
005900030110     d RecAR5          s              8  0   inz
006000000419     d RECAR6          s              8  0   inz
006100000419     d RECAR7          s              8  0   inz
006200000419     d RECBLT          s              8  0   inz
006300000414     D Wdata           S               D   DATFMT(*eur)
006400000413      *--------------------------------------------------------------*
006500000413      *
006600000419      * Se il membro da trascodificare non è "SEDE" vado a fine programma
006700000419     C                   If        MBRFIL = 'SEDE'
006800000419      * Apertura membri
006900900526     C                   EXSR      CAEXA
007000000413      *
007100000419      * Proseguo se non ho avuto errori in apertura
007200000413    1C                   IF        *in91 = *off
007300000414      *
007400000419      * Elaborazione record bolla ricevuti
007500000413     C     *LOVAL        SETLL     fnblp00r
007600000413     C                   READ      fnblp00r
007700000419    2C                   dow       not %EOF(fnblp00r)
007800000413     C                   EXSR      aggbol
007900000413     C                   READ      fnblp00r
008000000419    2C                   enddo
008100000419      *
008200000419      * Elaborazione legami ricevuti
008300041122     C**   *LOVAL        SETLL     fnlbl00r
008400041122     C**                 READ      fnlbl00r
008500041122    2C**                 dow       not %EOF(fnlbl00r)
008600041122     C**                 EXSR      aggleg
008700041122     C**                 READ      fnlbl00r
008800041122    2C**                 enddo
008900000413      *
009000000419      * Chiusura membri e aggiornamento archivi di log
009100041122     C**                 EXSR      AGGcmric
009200000413      *
009300000413     C                   Endif
009400000414     C                   Endif
009500000413      *
009600000413     C                   EVAL      *INLR = *ON
009700000413      **-------------------------------------------------------------**
009800000413      **  AGGIORNAMENTO FILES BOLLE                                  **
009900000413      **-------------------------------------------------------------**
010000950622     C     AGGBOL        BEGSR
010100000413      *
010200950623     C                   ADD       1             RECBLP
010300000413      *
010400021121     C     KBLP          chain     FNBLP000
010500000417      *
010600021121    0C                   IF        not %found(fnblp01l)
010700021121     c                   eval      ds_fnblp_o = ds_fnblp
010800000417      * FNBLP00F
010900021121     C                   z-add     udate8g       oBLPdt1
011000021121     C                   z-add     udate8g       oBLPdt2
011100021121     c                   movel     'R'           oBLPft1
011200021121     c                   movel     'R'           oBLPft2
011300000413     C                   WRITE     fnblp000
011400000413      *
011500060221      * FIAR400F
011600060221     C     KBLP          SETLL     FIAR400R
011700060221    1C                   IF        %EQUAL(fiar400r)
011800000414      *
011900060221     C     KBLP          DELETE    Fiar4000
012000060221     C                   DOW       %FOUND(fiar401l)
012100060221     C     KBLP          DELETE    Fiar4000
012200000414     C                   ENDDO
012300000414      *
012400060221     C     KBLP          READE     Fiar400R
012500000413      *
012600060221    2C                   DOW       not %EOF(fiar400r)
012700060221     C                   z-add     udate8g       ar4dtr
012800060221     c                   movel     'R'           ar4ftr
012900000502      * monitorizzo l'errore in quanto nel file di ricezione potrei avere i
013000000502      * record doppi (2 richieste di ripristino consecutive)
013100060221     C                   WRITE     Fiar4000                             31
013200060221     C                   DELETE    Fiar4R
013300060221     C     KBLP          READE     Fiar400R
013400950622    2C                   ENDDO
013500000413      *
013600950622    1C                   ENDIF
013700030110      *
013800030110      * FIAR500F
013900030110     C     KBLP          setll     FIAR500R
014000030110if  1C                   If        %EQUAL(FIAR500R)
014100030110      *
014200030110do  2C                   Dou       not %FOUND(FIAR501L)
014300030110     C     KBLP          DELETE    FIAR5000
014400030110e   2C                   Enddo
014500030110      *
014600030110     C     KBLP          reade     FIAR500R
014700030110do  2C                   Dow       not %EOF(FIAR500R)
014800030110     C                   add       1             RecAR5
014900030110     C                   movel     'R'           AR5ft1
015000030110     C                   z-add     udate8g       AR5dt1
015100030130     C                   movel     'R'           AR5ft2
015200030130     C                   z-add     udate8g       AR5dt2
015300040112     C                   WRITE     FIAR5000                             31
015400030110     C                   DELETE    FIAR5R
015500030110     C     KBLP          reade     FIAR500R
015600030110    2C                   Enddo
015700030110      *
015800030110    1C                   Endif
015900000414      *
016000000414      * FIAR600F
016100000426     C     KBLP          SETLL     FIAR600R
016200000426    0C                   IF        %EQUAL(fiar600r)
016300000426     C                   EXSR      AggTassaz
016400000426    1C                   ENDIF
016500000414      *
016600051116      * Fiar900F
016700051116     C     KBLP          CHAIN     Fiar900R
016800051116    0C                   IF        %FOUND(fiar900r)
016900000426      *
017000051116     C     KBLP          SETLL     Fiar901L
017100051116    0C                   if        not %EQUAL(fiar901l)
017200051116     C                   WRITE     Fiar9000
017300000426    1C                   endif
017400051116     C                   DELETE    Fiar9R
017500000414      *
017600000414    1C                   ENDIF
017700000414      *
017800000414      * FNBLT00F
017900000414     C     KBLP          SETLL     FNBLT00R
018000000414    0C                   IF        %EQUAL(fnblt00r)
018100000427     C                   EXSR      AggSegnac
018200000427     C                   ENDIF
018300000414      *
018400000414      * FNEVB00F
018500040831     C***  KBLP          SETLL     FNEVB00R
018600040831    0C****               IF        %EQUAL(fnevb00r)
018700000414      *
018800040831     C***  KBLP          READE     FNEVB00R
018900000414      *
019000040831    2C****               DOW       not %EOF(fnevb00r)
019100040831     C****               add       1             RECEVB
019200040831     C****               z-add     udate8g       EVBdtr
019300040831     c****               movel     'R'           EVBftr
019400040831     C****               WRITE     FNEVB000
019500040831     C****               DELETE    FNEVBR
019600040831     C**** KBLP          READE     FNEVB00R
019700040831    2C****               ENDDO
019800000414      *
019900040831    1C****               ENDIF
020000050909
020100050909      * Controllo se la bolla che ho appena rigenerato ha giacenza fleggata
020200050909      * solo per la sede in questo caso è da sfleggare
020300050909     c     kblp          Setll     Tigcp51l
020400050909     c                   Do        *Hival
020500050909     c     kblp          Reade     Tigcp51l
020600050909     c                   If        %Eof(Tigcp51l)
020700050909     c                   Leave
020800050909     c                   EndIf
020900050909     c                   If        gcpsede = 'S'
021000050909     c                   Clear                   gcpsede
021100050909     c                   Update    tigcp000
021200050909     c                   EndIf
021300050909     c                   EndDo
021400000413      *
021500000419      * CANCELLO IL RECORD RICEVUTO DI FNBLP00R
021600950630     C                   DELETE    FNBLPR
021700000414      *
021800950630   X0C                   ELSE
021900020205     c* SE ESISTE LA BOLLA e
022000060221      * ricevo il record 9 nel file fiar400r sono nel caso di c/assegni
022100020205      * omessi
022200020205     c                   clear                   flgcao            1
022300060221     c     kblp          setll     fiar400r
022400020205     c                   do        *hival
022500060221     c     kblp          reade     fiar400r
022600060221    0C                   if        %Eof(fiar400r)
022700020205     c                   leave
022800020205     c                   end
022900060221    0C                   if        ar4trc = '9' and
023000060221    0C                             %subst(ar4not: 1: 25)='PER C/ASSEGNO OMESSO'
023100020205     c                   movel     '1'           flgcao
023200020205     c                   leave
023300020205     c                   end
023400020205     c                   enddo
023500020205     c*
023600020205     c                   if        flgcao = '1'
023700020205     C                   EXSR      srcass
023800020205     c                   end
023900000419      * Cancello i record se la bolla è già presente in FNBLP00F
024000000417     C                   EXSR      DELrcd
024100000417      *
024200950630    0C                   ENDIF
024300000417      *
024400900530     C                   ENDSR
024500000426      **-------------------------------------------------------------**
024600000426      **  AGGIORNAMENTO FILE TASSAZIONE                              **
024700000426      **-------------------------------------------------------------**
024800000426     C     AGGTASSAZ     BEGSR
024900000426      *
025000000426     c                   clear                   DS_fiar6_O
025100000426     c                   clear                   DS_fiar6
025200000426     C     KBLP          READE     FIAR600R
025300000426      *
025400000426    2C                   DOW       not %EOF(fiar600r)
025500000426      *
025600000426      *  Aggancio con il tipo rcd il file effettivo
025700000426     C     KAR6          CHAIN     FIAR601L
025800000426      *
025900000426      *  Se lo trovo e non ha tassazione mentre quello ricevuto si allora
026000000426      *   cancello i record effettivi
026100000426     c                   IF        %FOUND(fiar601l)   and
026200000426     c                             oAR6div = *blanks  and  AR6div <> *blanks
026300000426     C                   DELETE    FIAR601L
026400000426     C     KAR6          DELETE    FIAR701L
026500000426    2C                   DOW       %FOUND(fiar701L)
026600000426     C     KAR6          DELETE    FIAR701L
026700000426    2C                   ENDDO
026800000426     C                   ENDIF
026900000426      *
027000000426      *  Se LO TROVO e non ha tassazione mentre quello ricevuto si  OPPURE
027100000426      *  NON LO TROVO scrivo FIAR6 e FIAR7 del tipo rcd in esame
027200000426     c                   IF        (%FOUND(fiar601l) and
027300000426     c                             oAR6div = *blanks and AR6div <> *blanks)
027400000426     c                               OR  NOT %FOUND(fiar601l)
027500000426     c                   EVAL      DS_fiar6_O = DS_fiar6
027600000502     C                   WRITE     FIAR6000
027700000426      *
027800000426     C     KAR6          SETLL     FIAR700R
027900000426     C     KAR6          READE     FIAR700R
028000000426    2C                   DOW       not %EOF(fiar700r)
028100000426     C                   ADD       1             RECAR7
028200000608     C                   WRITE     FIAR7000                             31
028300000426     C                   DELETE    FIAR7R
028400000426     C     KAR6          READE     FIAR700R
028500000426    2C                   ENDDO
028600000426     c                   ENDIF
028700000426      *
028800000426      *   Cancello FIAR6 ricevuto
028900000426     C                   add       1             RECAR6
029000000426     C                   DELETE    FIAR6R
029100000426      *
029200000426      *   Cancello FIAR7 ricevuti se non già fatto
029300000426     C     KAR6          delete    FIAR700R
029400000426    2C                   DOW       %FOUND(fiar700r)
029500000426     C                   add       1             RECAR7
029600000426     C     KAR6          delete    FIAR7R
029700000426    2C                   ENDDO
029800000426      *
029900000426     c                   clear                   DS_fiar6_O
030000000426     c                   clear                   DS_fiar6
030100000426     C     KBLP          READE     FIAR600R
030200000426     c                   ENDDO
030300000426      *
030400000426     C                   ENDSR
030500000427      **-------------------------------------------------------------**
030600000427      **  AGGIORNAMENTO FILE SEGNACOLLI                              **
030700000427      **-------------------------------------------------------------**
030800000427     C     AGGSEGNAC     BEGSR
030900000427      *
031000000427     c                   clear                   Wsegna
031100000427      * Verifico se tutti i segnacolli della spedizione sono univoci
031200000427     C     KBLP          READE     FNBLT00R
031300000427      *
031400000427    2C                   DOW       not %EOF(fnblt00r)
031500000427     C     Kblt          SETLL     FNBLT000
031600000427     C                   IF        %equal(fnblt27l)
031700000427     c                   EVAL      Wsegna = 'S'
031800000427     C                   LEAVE
031900000427     C                   ENDIF
032000000427     C     KBLP          READE     FNBLT00R
032100000427    2C                   ENDDO
032200000427      *
032300000427      * Se i segnacolli non sono univoci cancello i record ricevuti
032400000427     C                   IF        WSEGNA = 'S'
032500000427      *
032600000427     C     KBLP          DELETE    FNBLTR
032700000427     C                   DOW       %FOUND(fnblt00r)
032800000427     C                   add       1             RECBLT
032900000427     C     KBLP          DELETE    FNBLTR
033000000427     C                   ENDDO
033100000427      *
033200000427      * Se i segnacolli sono univoci aggiorno il file effettivo
033300000427     C                   ELSE
033400000427      *
033500000427     C     KBLP          SETLL     FNBLT00R
033600000427     C     KBLP          READE     FNBLT00R
033700000427      *
033800000427    2C                   DOW       not %EOF(fnblt00r)
033900000427     C                   add       1             RECBLT
034000000427     C                   z-add     udate8g       BLTdtr
034100000427     c                   movel     'R'           BLTftr
034200000608     C                   WRITE     FNBLT000                             31
034300000427     C                   DELETE    FNBLTR
034400000427     C     KBLP          READE     FNBLT00R
034500000427    2C                   ENDDO
034600000427      *
034700000427     C                   ENDIF
034800000427      *
034900000427     C                   ENDSR
035000000419      **-------------------------------------------------------------**
035100000419      **  AGGIORNAMENTO FILE LEGAMI                                  **
035200000419      **-------------------------------------------------------------**
035300041122     C**   AGGLEG        BEGSR
035400000419      *
035500041122     C**                 ADD       1             RECLBL
035600041122     C**   KLBL          SETLL     FNLBL02L
035700000419      *
035800041122    0C**                 IF        not %EQUAL(fnlbl02L)
035900041122     C**                 WRITE     FNLBL000
036000041122    1C**                 ENDIF
036100000419      *
036200041122     C**                 DELETE    FNLBLR
036300000419      *
036400041122     C**                 ENDSR
036500020205      **-------------------------------------------------------------**
036600020205      **  AGGIORNAMENTO FILES x C/ASSEGNI OMESSI                     **
036700020205      **-------------------------------------------------------------**
036800020205     C     srcass        BEGSR
036900060221      * Fiar400F solo record 8 e 9
037000060221     C     Kblp          SETLL     Fiar400R
037100020205     c                   do        *hival
037200060221     C     Kblp          READE     Fiar400R
037300060221    0C                   IF        %Eof(fiar400r)
037400020205     c                   leave
037500020205     c                   end
037600060221    0C                   IF        ar4trc <> '8' and ar4trc <> '9'
037700020205     c                   iter
037800020205     c                   end
037900060221     C     KBL89         DELETE    Fiar4000
038000060221     C                   z-add     udate8g       ar4dtr
038100060221     c                   movel     'R'           ar4ftr
038200020205      * monitorizzo l'errore in quanto nel file di ricezione potrei avere i
038300020205      * record doppi (2 richieste di ripristino consecutive)
038400060221     C                   WRITE     Fiar4000                             31
038500020205    2C                   ENDDO
038600051116      * FIAR900F
038700051116     C     KBLP          CHAIN     Fiar900R
038800051116    0C                   IF        %FOUND(fiar900r)
038900051116     C     KBLP          SETLL     Fiar901L
039000051116    0C                   if        not %EQUAL(fiar901l)
039100051116     C                   WRITE     Fiar9000
039200020205    1C                   endif
039300020205    1C                   ENDIF
039400021121      * FNBLP00F
039500021121     c                   movel     blpcbo        oblpcbo
039600021121     c                   update    fnblp000
039700020205      *
039800020205     C                   ENDSR
039900000417      **-------------------------------------------------------------**
040000000417      **  CANCELLAZIONE BOLLE RICEVUTE                               **
040100000417      **-------------------------------------------------------------**
040200000417     C     DELRCD        BEGSR
040300000417      *
040400060221      * Fiar400F
040500060221     C     KBLP          DELETE    Fiar4R
040600060221     C                   DOW       %FOUND(fiar400r)
040700060221     C     KBLP          DELETE    Fiar4R
040800000417     C                   ENDDO
040900030110      *
041000030110      * FIAR500F
041100030110     C     KBLP          DELETE    FIAR5R
041200030110     C                   Dow       %FOUND(FIAR500R)
041300030110     C                   add       1             RecAR5
041400030110     C     KBLP          DELETE    FIAR5R
041500030110     C                   Enddo
041600000417      *
041700000417      * FIAR600F
041800000417     C     KBLP          DELETE    FIAR6R
041900000419     C                   DOW       %FOUND(fiar600r)
042000000417     C                   add       1             RECAR6
042100000417     C     KBLP          DELETE    FIAR6R
042200000417     C                   ENDDO
042300000417      *
042400000417      * FIAR700F
042500000417     C     KBLP          DELETE    FIAR7R
042600000419     C                   DOW       %FOUND(fiar700r)
042700000417     C                   add       1             RECAR7
042800000417     C     KBLP          DELETE    FIAR7R
042900000417     C                   ENDDO
043000000417      *
043100051116      * FiAR900F
043200051116     C     KBLP          DELETE    Fiar9R
043300051116     C                   DOW       %FOUND(fiar900r)
043400051116     C     KBLP          DELETE    Fiar9R
043500000417     C                   ENDDO
043600000417      *
043700000417      * FNBLT00F
043800000417     C     KBLP          DELETE    FNBLTR
043900000419     C                   DOW       %FOUND(fnblt00r)
044000000417     C                   add       1             RECBLT
044100000417     C     KBLP          DELETE    FNBLTR
044200000417     C                   ENDDO
044300000417      *
044400000417      * FNEVB00F
044500040831     C***  KBLP          DELETE    FNEVBR
044600040831     C***                DOW       %FOUND(fnevb00r)
044700040831     C***                add       1             RECEVB
044800040831     C***  KBLP          DELETE    FNEVBR
044900040831     C***                ENDDO
045000000417      *
045100000417      * FNBLP00F
045200000417     C                   DELETE    FNBLPR
045300000417      *
045400000417     C                   ENDSR
045500000413      **-------------------------------------------------------------**
045600000413      **  ESEGUE APERTURA SUI MEMBRI                                 **
045700000413      **-------------------------------------------------------------**
045800900526     C     CAEXA         BEGSR
045900000413      *
046000000413     c                   Do        Wnumele       XX
046100000413      *
046200000419      * Aggiungo il membro
046300000419     C                   MOVEL(P)  ScAdd(xx)     comman
046400000419     C                   CALL      'QCMDEXC'                            31
046500000413     C                   PARM                    comman
046600000413     C                   PARM                    lung
046700000413      *
046800000419      * Alloco il membro, se non riesco chiudo il PGM
046900000419     C                   MOVEL(P)  ScAlc(xx)     comman
047000000419     C                   CALL      'QCMDEXC'                            91
047100000419     C                   PARM                    comman
047200000419     C                   PARM                    lung
047300000419      *
047400000413     C   91              LEAVE
047500000413      *
047600000419      * Eseguo l'ovrdbf, se non riesco chiudo il pgm
047700000413     C                   MOVEL(P)  ScOvr(xx)     comman
047800000419     C                   CALL      'QCMDEXC'                            91
047900000413     C                   PARM                    comman
048000000413     C                   PARM                    lung
048100000413      *
048200000419     C   91              LEAVE
048300000419      *
048400000419     C                   ENDDO
048500000419      *
048600000413      * Apro i file
048700000419     C                   IF        *IN91 = *off
048800000413     C                   open      FNBLP00R
048900060221     C                   open      Fiar400R
049000030110     C                   open      FIAR500R
049100000413     C                   open      FIAR600R
049200000413     C                   open      FIAR700R
049300051116     C                   open      Fiar900R
049400000413     C                   open      FNBLT00R
049500041122     C***                open      FNLBL00R
049600040831     C****               open      FNEVB00R
049700000419     C                   ENDIF
049800000413      *
049900000413     C                   ENDSR
050000000419      *---------------------------------------------------------------*
050100000419      * Chiusura membro trascodificato e aggiornamento archivi di log
050200000419      *---------------------------------------------------------------*
050300000419     C     AGGcmric      BEGSR
050400000419      *
050500000419     C                   close     FNBLP00R
050600060221     C                   close     Fiar400R
050700030110     C                   close     FIAR500R
050800000419     C                   close     FIAR600R
050900000419     C                   close     FIAR700R
051000051116     C                   close     Fiar900R
051100000419     C                   close     FNBLT00R
051200041122     C***                close     FNLBL00R
051300040831     C****               close     FNEVB00R
051400000419      *
051500000419     C                   ENDSR
051600000413      *---------------------------------------------------------------*
051700000413      * SUBROUTINE INIZIALE
051800000413      *---------------------------------------------------------------*
051900000413     C     *INZSR        BEGSR
052000000413      *
052100000413     C     *ENTRY        PLIST
052200000413     C                   PARM                    KPJBA
052300000413      *
052400000413     c                   EVAL      Wnumele = %elem(ScAlc)
052500000413      *
052600000413     C                   Z-ADD     1             CODUT
052700000413     C                   CALL      'X§PARUT'
052800000413     C                   PARM                    UT§DSE0F
052900000413     C                   MOVEL     RAGUT         RSUT
053000000413     C                   MOVEL     REC80         CNCR80
053100000413      *
053200000413      * LA DATA DEL GIORNO LA PRENDO DA TIME
053300000413     C                   TIME                    W0140
053400000413     C                   MOVE      W0140         UDATE8
053500000414      * UDATE IN AAAAMMGG
053600000414     C     *eur          MOVEL     UDATE8        Wdata
053700000414     C     *iso          MOVEL     Wdata         UDATE8G
053800000413      *
053900041122     C**   KLBL          KLIST
054000041122     C**                 KFLD                    LBLAAP
054100041122     C**                 KFLD                    LBLLPP
054200041122     C**                 KFLD                    LBLNRP
054300041122     C**                 KFLD                    LBLNSP
054400041122     C**                 KFLD                    LBLAAN
054500041122     C**                 KFLD                    LBLLPN
054600041122     C**                 KFLD                    LBLNRN
054700041122     C**                 KFLD                    LBLNSN
054800000419      *
054900000419     C     KBLP          KLIST
055000000419     C                   KFLD                    BLPAAS
055100000419     C                   KFLD                    BLPLNP
055200000419     C                   KFLD                    BLPNRS
055300000419     C                   KFLD                    BLPNSP
055400020205      *
055500020205     C     KBL89         KLIST
055600020205     C                   KFLD                    blPAAS
055700020205     C                   KFLD                    blPLNP
055800020205     C                   KFLD                    blPNRS
055900020205     C                   KFLD                    blPNSP
056000060221     C                   KFLD                    ar4TRC
056100000426      *
056200000426     C     KAR6          KLIST
056300000426     C                   KFLD                    ar6AAS
056400000426     C                   KFLD                    ar6LNP
056500000426     C                   KFLD                    ar6NRS
056600000426     C                   KFLD                    ar6NSP
056700000426     C                   KFLD                    ar6TRC
056800000427      *
056900000427     C     KBLT          KLIST
057000000427     C                   KFLD                    bltFLS
057100000427     C                   KFLD                    bltLNA
057200000427     C                   KFLD                    bltNRS
057300000427     C                   KFLD                    bltNSC
057400000413      *
057500000413     C                   ENDSR
057600000413      *---------------------------------------------------------------*
057700000419** SCADD
057800000419ADDPFM FILE(FNBLP00R  )                 MBR(SEDE)
057900060221ADDPFM FILE(FIAR400R  )                 MBR(SEDE)
058000030110ADDPFM FILE(FIAR500R  )                 MBR(SEDE)
058100000419ADDPFM FILE(FIAR600R  )                 MBR(SEDE)
058200000419ADDPFM FILE(FIAR700R  )                 MBR(SEDE)
058300051116ADDPFM FILE(FIAR900R  )                 MBR(SEDE)
058400000419ADDPFM FILE(FNBLT00R  )                 MBR(SEDE)
058500000413** SCALC
058600000413ALCOBJ OBJ((FNBLP00R *FILE *EXCL           SEDE))
058700060221ALCOBJ OBJ((FIAR400R *FILE *EXCL           SEDE))
058800030110ALCOBJ OBJ((FIAR500R *FILE *EXCL           SEDE))
058900000413ALCOBJ OBJ((FIAR600R *FILE *EXCL           SEDE))
059000000413ALCOBJ OBJ((FIAR700R *FILE *EXCL           SEDE))
059100051116ALCOBJ OBJ((FIAR900R *FILE *EXCL           SEDE))
059200000413ALCOBJ OBJ((FNBLT00R *FILE *EXCL           SEDE))
059300000413** SCOVR
059400000419OVRDBF FILE(FNBLP00R  )                 MBR(SEDE)
059500060221OVRDBF FILE(FIAR400R  )                 MBR(SEDE)
059600030110OVRDBF FILE(FIAR500R  )                 MBR(SEDE)
059700000419OVRDBF FILE(FIAR600R  )                 MBR(SEDE)
059800000419OVRDBF FILE(FIAR700R  )                 MBR(SEDE)
059900051116OVRDBF FILE(FIAR900R  )                 MBR(SEDE)
060000000419OVRDBF FILE(FNBLT00R  )                 MBR(SEDE)
