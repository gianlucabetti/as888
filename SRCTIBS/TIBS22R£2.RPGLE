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
003100060303     Ftabel00f  IF A E           K DISK
003200041122     F****FNLBL02L  UF A E           K DISK
003300040831     F****FNEVB01L  UF A E           K DISK
003400050909     fTigcp51l  uf   e           k disk
003500000413      *
003600041122     D SCadd           S             55    DIM(7) CTDATA PERRCD(1)
003700041122     D SCalc           S             55    DIM(7) CTDATA PERRCD(1)
003800041122     D SCovr           S             55    DIM(7) CTDATA PERRCD(1)
003900000413      *
004000000000     D KPJBA         E DS
004100900526     D  LEGFIL               483    492
004200900526     D  MBRFIL               493    502
004300000413     D UT§DSE0F      E DS
004400991126     D CNCR80        E DS
004500060303     D ds3a          E DS
004600000426     D DS_FIAR6      E DS                  extname(FIAR600F)
004700000426     D DS_FIAR6_O    E DS                  extname(FIAR600F) prefix(o)
004800021121     D DS_Fnblp      E DS                  extname(Fnblp00F)
004900021121     D DS_Fnblp_O    E DS                  extname(Fnblp00F) prefix(o)
005000000413      *
005100000414     D XX              S              3  0
005200000414     D lung            S             15  5 INZ(55)
005300000413     D comman          S             80
005400000413     D Wnumele         S              5P 0
005500000413     D RSUT            S             20
005600000427     D Wsegna          S              1
005700000413     D W0140           S             14  0
005800000413     D UDATE8          S              8  0
005900000414     D UDATE8G         S              8  0
006000000419     d RECBLP          s              8  0   inz
006100030110     d RecAR5          s              8  0   inz
006200000419     d RECAR6          s              8  0   inz
006300000419     d RECAR7          s              8  0   inz
006400000419     d RECBLT          s              8  0   inz
006500000414     D Wdata           S               D   DATFMT(*eur)
006600000413      *--------------------------------------------------------------*
006700000413      *
006800000419      * Se il membro da trascodificare non è "SEDE" vado a fine programma
006900000419     C                   If        MBRFIL = 'SEDE'
007000000419      * Apertura membri
007100900526     C                   EXSR      CAEXA
007200000413      *
007300000419      * Proseguo se non ho avuto errori in apertura
007400000413    1C                   IF        *in91 = *off
007500000414      *
007600000419      * Elaborazione record bolla ricevuti
007700000413     C     *LOVAL        SETLL     fnblp00r
007800000413     C                   READ      fnblp00r
007900000419    2C                   dow       not %EOF(fnblp00r)
008000000413     C                   EXSR      aggbol
008100000413     C                   READ      fnblp00r
008200000419    2C                   enddo
008300000419      *
008400000419      * Elaborazione legami ricevuti
008500041122     C**   *LOVAL        SETLL     fnlbl00r
008600041122     C**                 READ      fnlbl00r
008700041122    2C**                 dow       not %EOF(fnlbl00r)
008800041122     C**                 EXSR      aggleg
008900041122     C**                 READ      fnlbl00r
009000041122    2C**                 enddo
009100000413      *
009200000419      * Chiusura membri e aggiornamento archivi di log
009300041122     C**                 EXSR      AGGcmric
009400000413      *
009500000413     C                   Endif
009600000414     C                   Endif
009700000413      *
009800000413     C                   EVAL      *INLR = *ON
009900000413      **-------------------------------------------------------------**
010000000413      **  AGGIORNAMENTO FILES BOLLE                                  **
010100000413      **-------------------------------------------------------------**
010200950622     C     AGGBOL        BEGSR
010300000413      *
010400950623     C                   ADD       1             RECBLP
010500000413      *
010600021121     C     KBLP          chain     FNBLP000
010700000417      *
010800021121    0C                   IF        not %found(fnblp01l)
010900021121     c                   eval      ds_fnblp_o = ds_fnblp
011000000417      * FNBLP00F
011100021121     C                   z-add     udate8g       oBLPdt1
011200021121     C                   z-add     udate8g       oBLPdt2
011300021121     c                   movel     'R'           oBLPft1
011400021121     c                   movel     'R'           oBLPft2
011500000413     C                   WRITE     fnblp000
011600000413      *
011700060221      * FIAR400F
011800060221     C     KBLP          SETLL     FIAR400R
011900060221    1C                   IF        %EQUAL(fiar400r)
012000000414      *
012100060221     C     KBLP          DELETE    Fiar4000
012200060221     C                   DOW       %FOUND(fiar401l)
012300060221     C     KBLP          DELETE    Fiar4000
012400000414     C                   ENDDO
012500000414      *
012600060221     C     KBLP          READE     Fiar400R
012700000413      *
012800060221    2C                   DOW       not %EOF(fiar400r)
012900060221     C                   z-add     udate8g       ar4dtr
013000060221     c                   movel     'R'           ar4ftr
013100000502      * monitorizzo l'errore in quanto nel file di ricezione potrei avere i
013200000502      * record doppi (2 richieste di ripristino consecutive)
013300060221     C                   WRITE     Fiar4000                             31
013400060221     C                   DELETE    Fiar4R
013500060221     C     KBLP          READE     Fiar400R
013600950622    2C                   ENDDO
013700000413      *
013800950622    1C                   ENDIF
013900030110      *
014000030110      * FIAR500F
014100030110     C     KBLP          setll     FIAR500R
014200030110if  1C                   If        %EQUAL(FIAR500R)
014300030110      *
014400030110do  2C                   Dou       not %FOUND(FIAR501L)
014500030110     C     KBLP          DELETE    FIAR5000
014600030110e   2C                   Enddo
014700030110      *
014800030110     C     KBLP          reade     FIAR500R
014900030110do  2C                   Dow       not %EOF(FIAR500R)
015000030110     C                   add       1             RecAR5
015100030110     C                   movel     'R'           AR5ft1
015200030110     C                   z-add     udate8g       AR5dt1
015300030130     C                   movel     'R'           AR5ft2
015400030130     C                   z-add     udate8g       AR5dt2
015500040112     C                   WRITE     FIAR5000                             31
015600030110     C                   DELETE    FIAR5R
015700030110     C     KBLP          reade     FIAR500R
015800030110    2C                   Enddo
015900030110      *
016000030110    1C                   Endif
016100000414      *
016200000414      * FIAR600F
016300000426     C     KBLP          SETLL     FIAR600R
016400000426    0C                   IF        %EQUAL(fiar600r)
016500000426     C                   EXSR      AggTassaz
016600000426    1C                   ENDIF
016700000414      *
016800051116      * Fiar900F
016900051116     C     KBLP          CHAIN     Fiar900R
017000051116    0C                   IF        %FOUND(fiar900r)
017100000426      *
017200051116     C     KBLP          SETLL     Fiar901L
017300051116    0C                   if        not %EQUAL(fiar901l)
017400051116     C                   WRITE     Fiar9000
017500000426    1C                   endif
017600051116     C                   DELETE    Fiar9R
017700000414      *
017800000414    1C                   ENDIF
017900000414      *
018000000414      * FNBLT00F
018100000414     C     KBLP          SETLL     FNBLT00R
018200000414    0C                   IF        %EQUAL(fnblt00r)
018300000427     C                   EXSR      AggSegnac
018400000427     C                   ENDIF
018500000414      *
018600000414      * FNEVB00F
018700040831     C***  KBLP          SETLL     FNEVB00R
018800040831    0C****               IF        %EQUAL(fnevb00r)
018900000414      *
019000040831     C***  KBLP          READE     FNEVB00R
019100000414      *
019200040831    2C****               DOW       not %EOF(fnevb00r)
019300040831     C****               add       1             RECEVB
019400040831     C****               z-add     udate8g       EVBdtr
019500040831     c****               movel     'R'           EVBftr
019600040831     C****               WRITE     FNEVB000
019700040831     C****               DELETE    FNEVBR
019800040831     C**** KBLP          READE     FNEVB00R
019900040831    2C****               ENDDO
020000000414      *
020100040831    1C****               ENDIF
020200050909
020300050909      * Controllo se la bolla che ho appena rigenerato ha giacenza fleggata
020400050909      * solo per la sede in questo caso è da sfleggare
020500050909     c     kblp          Setll     Tigcp51l
020600050909     c                   Do        *Hival
020700050909     c     kblp          Reade     Tigcp51l
020800050909     c                   If        %Eof(Tigcp51l)
020900050909     c                   Leave
021000050909     c                   EndIf
021100050909     c                   If        gcpsede = 'S'
021200050909     c                   Clear                   gcpsede
021300050909     c                   Update    tigcp000
021400050909     c                   EndIf
021500050909     c                   EndDo
021600000413      *
021700000419      * CANCELLO IL RECORD RICEVUTO DI FNBLP00R
021800950630     C                   DELETE    FNBLPR
021900000414      *
022000950630   X0C                   ELSE
022100020205     c* SE ESISTE LA BOLLA e
022200060221      * ricevo il record 9 nel file fiar400r sono nel caso di c/assegni
022300020205      * omessi
022400020205     c                   clear                   flgcao            1
022500060221     c     kblp          setll     fiar400r
022600060303    1c                   do        *hival
022700060221     c     kblp          reade     fiar400r
022800060303    2C                   if        %Eof(fiar400r)
022900020205     c                   leave
023000060303    2c                   end
023100060303    2C                   if        ar4trc = '9' and
023200060303     C                             %subst(ar4not: 1: 25)='PER C/ASSEGNO OMESSO'
023300020205     c                   movel     '1'           flgcao
023400020205     c                   leave
023500060303    2c                   end
023600060303    1c                   enddo
023700060303     c*
023800060303    1c                   if        flgcao=' '
023900060303     C                   EVAL      AR4TRC='9'
024000060303     c     kbl89         CHAIN(N)  fiar401l
024100060303    2C                   if        %FOUND (fiar401l) AND
024200060303     C                             %subst(ar4not: 1: 25)='PER C/ASSEGNO OMESSO'
024300060303     c                   movel     '1'           flgcao
024400060303    2c                   endif
024500060303    1c                   endif
024600020205     c*
024700020205     c                   if        flgcao = '1'
024800020205     C                   EXSR      srcass
024900020205     c                   end
025000000419      * Cancello i record se la bolla è già presente in FNBLP00F
025100000417     C                   EXSR      DELrcd
025200000417      *
025300950630    0C                   ENDIF
025400000417      *
025500900530     C                   ENDSR
025600000426      **-------------------------------------------------------------**
025700000426      **  AGGIORNAMENTO FILE TASSAZIONE                              **
025800000426      **-------------------------------------------------------------**
025900000426     C     AGGTASSAZ     BEGSR
026000000426      *
026100000426     c                   clear                   DS_fiar6_O
026200000426     c                   clear                   DS_fiar6
026300000426     C     KBLP          READE     FIAR600R
026400000426      *
026500000426    2C                   DOW       not %EOF(fiar600r)
026600000426      *
026700000426      *  Aggancio con il tipo rcd il file effettivo
026800000426     C     KAR6          CHAIN     FIAR601L
026900000426      *
027000000426      *  Se lo trovo e non ha tassazione mentre quello ricevuto si allora
027100000426      *   cancello i record effettivi
027200000426     c                   IF        %FOUND(fiar601l)   and
027300000426     c                             oAR6div = *blanks  and  AR6div <> *blanks
027400000426     C                   DELETE    FIAR601L
027500000426     C     KAR6          DELETE    FIAR701L
027600000426    2C                   DOW       %FOUND(fiar701L)
027700000426     C     KAR6          DELETE    FIAR701L
027800000426    2C                   ENDDO
027900000426     C                   ENDIF
028000000426      *
028100000426      *  Se LO TROVO e non ha tassazione mentre quello ricevuto si  OPPURE
028200000426      *  NON LO TROVO scrivo FIAR6 e FIAR7 del tipo rcd in esame
028300000426     c                   IF        (%FOUND(fiar601l) and
028400000426     c                             oAR6div = *blanks and AR6div <> *blanks)
028500000426     c                               OR  NOT %FOUND(fiar601l)
028600000426     c                   EVAL      DS_fiar6_O = DS_fiar6
028700000502     C                   WRITE     FIAR6000
028800000426      *
028900000426     C     KAR6          SETLL     FIAR700R
029000000426     C     KAR6          READE     FIAR700R
029100000426    2C                   DOW       not %EOF(fiar700r)
029200000426     C                   ADD       1             RECAR7
029300000608     C                   WRITE     FIAR7000                             31
029400000426     C                   DELETE    FIAR7R
029500000426     C     KAR6          READE     FIAR700R
029600000426    2C                   ENDDO
029700000426     c                   ENDIF
029800000426      *
029900000426      *   Cancello FIAR6 ricevuto
030000000426     C                   add       1             RECAR6
030100000426     C                   DELETE    FIAR6R
030200000426      *
030300000426      *   Cancello FIAR7 ricevuti se non già fatto
030400000426     C     KAR6          delete    FIAR700R
030500000426    2C                   DOW       %FOUND(fiar700r)
030600000426     C                   add       1             RECAR7
030700000426     C     KAR6          delete    FIAR7R
030800000426    2C                   ENDDO
030900000426      *
031000000426     c                   clear                   DS_fiar6_O
031100000426     c                   clear                   DS_fiar6
031200000426     C     KBLP          READE     FIAR600R
031300000426     c                   ENDDO
031400000426      *
031500000426     C                   ENDSR
031600000427      **-------------------------------------------------------------**
031700000427      **  AGGIORNAMENTO FILE SEGNACOLLI                              **
031800000427      **-------------------------------------------------------------**
031900000427     C     AGGSEGNAC     BEGSR
032000000427      *
032100000427     c                   clear                   Wsegna
032200000427      * Verifico se tutti i segnacolli della spedizione sono univoci
032300000427     C     KBLP          READE     FNBLT00R
032400000427      *
032500000427    2C                   DOW       not %EOF(fnblt00r)
032600000427     C     Kblt          SETLL     FNBLT000
032700000427     C                   IF        %equal(fnblt27l)
032800000427     c                   EVAL      Wsegna = 'S'
032900000427     C                   LEAVE
033000000427     C                   ENDIF
033100000427     C     KBLP          READE     FNBLT00R
033200000427    2C                   ENDDO
033300000427      *
033400000427      * Se i segnacolli non sono univoci cancello i record ricevuti
033500000427     C                   IF        WSEGNA = 'S'
033600000427      *
033700000427     C     KBLP          DELETE    FNBLTR
033800000427     C                   DOW       %FOUND(fnblt00r)
033900000427     C                   add       1             RECBLT
034000000427     C     KBLP          DELETE    FNBLTR
034100000427     C                   ENDDO
034200000427      *
034300000427      * Se i segnacolli sono univoci aggiorno il file effettivo
034400000427     C                   ELSE
034500000427      *
034600000427     C     KBLP          SETLL     FNBLT00R
034700000427     C     KBLP          READE     FNBLT00R
034800000427      *
034900000427    2C                   DOW       not %EOF(fnblt00r)
035000000427     C                   add       1             RECBLT
035100000427     C                   z-add     udate8g       BLTdtr
035200000427     c                   movel     'R'           BLTftr
035300000608     C                   WRITE     FNBLT000                             31
035400000427     C                   DELETE    FNBLTR
035500000427     C     KBLP          READE     FNBLT00R
035600000427    2C                   ENDDO
035700000427      *
035800000427     C                   ENDIF
035900000427      *
036000000427     C                   ENDSR
036100000419      **-------------------------------------------------------------**
036200000419      **  AGGIORNAMENTO FILE LEGAMI                                  **
036300000419      **-------------------------------------------------------------**
036400041122     C**   AGGLEG        BEGSR
036500000419      *
036600041122     C**                 ADD       1             RECLBL
036700041122     C**   KLBL          SETLL     FNLBL02L
036800000419      *
036900041122    0C**                 IF        not %EQUAL(fnlbl02L)
037000041122     C**                 WRITE     FNLBL000
037100041122    1C**                 ENDIF
037200000419      *
037300041122     C**                 DELETE    FNLBLR
037400000419      *
037500041122     C**                 ENDSR
037600020205      **-------------------------------------------------------------**
037700020205      **  AGGIORNAMENTO FILES x C/ASSEGNI OMESSI                     **
037800020205      **-------------------------------------------------------------**
037900020205     C     srcass        BEGSR
038000060221      * Fiar400F solo record 8 e 9
038100060221     C     Kblp          SETLL     Fiar400R
038200020205     c                   do        *hival
038300060221     C     Kblp          READE     Fiar400R
038400060221    0C                   IF        %Eof(fiar400r)
038500020205     c                   leave
038600020205     c                   end
038700060221    0C                   IF        ar4trc <> '8' and ar4trc <> '9'
038800020205     c                   iter
038900020205     c                   end
039000060221     C     KBL89         DELETE    Fiar4000
039100060221     C                   z-add     udate8g       ar4dtr
039200060221     c                   movel     'R'           ar4ftr
039300020205      * monitorizzo l'errore in quanto nel file di ricezione potrei avere i
039400020205      * record doppi (2 richieste di ripristino consecutive)
039500060221     C                   WRITE     Fiar4000                             31
039600020205    2C                   ENDDO
039700051116      * FIAR900F
039800051116     C     KBLP          CHAIN     Fiar900R
039900051116    0C                   IF        %FOUND(fiar900r)
040000051116     C     KBLP          SETLL     Fiar901L
040100051116    0C                   if        not %EQUAL(fiar901l)
040200051116     C                   WRITE     Fiar9000
040300020205    1C                   endif
040400020205    1C                   ENDIF
040500021121      * FNBLP00F
040600060303     c*Aggiorno Oblpcbo, solo se il codice bolla non prevede C/A
040700060303     C                   MOVEL     '3A'          KCOD
040800060303     c                   movel(P)  oblpcbo       kkey
040900060303     C     KTAB1         CHAIN     TABEL00F                           30
041000060303     C  N30              MOVEL     TBLUNI        DS3a
041100060303     c   30              clear                   ds3a
041200060303     c                   if        §3afca=0
041300021121     c                   movel     blpcbo        oblpcbo
041400021121     c                   update    fnblp000
041500060303     c                   else
041600060303     c                   unlock    fnblp01l
041700060303     c                   endif
041800020205      *
041900020205     C                   ENDSR
042000000417      **-------------------------------------------------------------**
042100000417      **  CANCELLAZIONE BOLLE RICEVUTE                               **
042200000417      **-------------------------------------------------------------**
042300000417     C     DELRCD        BEGSR
042400000417      *
042500060221      * Fiar400F
042600060221     C     KBLP          DELETE    Fiar4R
042700060221     C                   DOW       %FOUND(fiar400r)
042800060221     C     KBLP          DELETE    Fiar4R
042900000417     C                   ENDDO
043000030110      *
043100030110      * FIAR500F
043200030110     C     KBLP          DELETE    FIAR5R
043300030110     C                   Dow       %FOUND(FIAR500R)
043400030110     C                   add       1             RecAR5
043500030110     C     KBLP          DELETE    FIAR5R
043600030110     C                   Enddo
043700000417      *
043800000417      * FIAR600F
043900000417     C     KBLP          DELETE    FIAR6R
044000000419     C                   DOW       %FOUND(fiar600r)
044100000417     C                   add       1             RECAR6
044200000417     C     KBLP          DELETE    FIAR6R
044300000417     C                   ENDDO
044400000417      *
044500000417      * FIAR700F
044600000417     C     KBLP          DELETE    FIAR7R
044700000419     C                   DOW       %FOUND(fiar700r)
044800000417     C                   add       1             RECAR7
044900000417     C     KBLP          DELETE    FIAR7R
045000000417     C                   ENDDO
045100000417      *
045200051116      * FiAR900F
045300051116     C     KBLP          DELETE    Fiar9R
045400051116     C                   DOW       %FOUND(fiar900r)
045500051116     C     KBLP          DELETE    Fiar9R
045600000417     C                   ENDDO
045700000417      *
045800000417      * FNBLT00F
045900000417     C     KBLP          DELETE    FNBLTR
046000000419     C                   DOW       %FOUND(fnblt00r)
046100000417     C                   add       1             RECBLT
046200000417     C     KBLP          DELETE    FNBLTR
046300000417     C                   ENDDO
046400000417      *
046500000417      * FNEVB00F
046600040831     C***  KBLP          DELETE    FNEVBR
046700040831     C***                DOW       %FOUND(fnevb00r)
046800040831     C***                add       1             RECEVB
046900040831     C***  KBLP          DELETE    FNEVBR
047000040831     C***                ENDDO
047100000417      *
047200000417      * FNBLP00F
047300000417     C                   DELETE    FNBLPR
047400000417      *
047500000417     C                   ENDSR
047600000413      **-------------------------------------------------------------**
047700000413      **  ESEGUE APERTURA SUI MEMBRI                                 **
047800000413      **-------------------------------------------------------------**
047900900526     C     CAEXA         BEGSR
048000000413      *
048100000413     c                   Do        Wnumele       XX
048200000413      *
048300000419      * Aggiungo il membro
048400000419     C                   MOVEL(P)  ScAdd(xx)     comman
048500000419     C                   CALL      'QCMDEXC'                            31
048600000413     C                   PARM                    comman
048700000413     C                   PARM                    lung
048800000413      *
048900000419      * Alloco il membro, se non riesco chiudo il PGM
049000000419     C                   MOVEL(P)  ScAlc(xx)     comman
049100000419     C                   CALL      'QCMDEXC'                            91
049200000419     C                   PARM                    comman
049300000419     C                   PARM                    lung
049400000419      *
049500000413     C   91              LEAVE
049600000413      *
049700000419      * Eseguo l'ovrdbf, se non riesco chiudo il pgm
049800000413     C                   MOVEL(P)  ScOvr(xx)     comman
049900000419     C                   CALL      'QCMDEXC'                            91
050000000413     C                   PARM                    comman
050100000413     C                   PARM                    lung
050200000413      *
050300000419     C   91              LEAVE
050400000419      *
050500000419     C                   ENDDO
050600000419      *
050700000413      * Apro i file
050800000419     C                   IF        *IN91 = *off
050900000413     C                   open      FNBLP00R
051000060221     C                   open      Fiar400R
051100030110     C                   open      FIAR500R
051200000413     C                   open      FIAR600R
051300000413     C                   open      FIAR700R
051400051116     C                   open      Fiar900R
051500000413     C                   open      FNBLT00R
051600041122     C***                open      FNLBL00R
051700040831     C****               open      FNEVB00R
051800000419     C                   ENDIF
051900000413      *
052000000413     C                   ENDSR
052100000419      *---------------------------------------------------------------*
052200000419      * Chiusura membro trascodificato e aggiornamento archivi di log
052300000419      *---------------------------------------------------------------*
052400000419     C     AGGcmric      BEGSR
052500000419      *
052600000419     C                   close     FNBLP00R
052700060221     C                   close     Fiar400R
052800030110     C                   close     FIAR500R
052900000419     C                   close     FIAR600R
053000000419     C                   close     FIAR700R
053100051116     C                   close     Fiar900R
053200000419     C                   close     FNBLT00R
053300041122     C***                close     FNLBL00R
053400040831     C****               close     FNEVB00R
053500000419      *
053600000419     C                   ENDSR
053700000413      *---------------------------------------------------------------*
053800000413      * SUBROUTINE INIZIALE
053900000413      *---------------------------------------------------------------*
054000000413     C     *INZSR        BEGSR
054100000413      *
054200000413     C     *ENTRY        PLIST
054300000413     C                   PARM                    KPJBA
054400000413      *
054500000413     c                   EVAL      Wnumele = %elem(ScAlc)
054600000413      *
054700000413     C                   Z-ADD     1             CODUT
054800000413     C                   CALL      'X§PARUT'
054900000413     C                   PARM                    UT§DSE0F
055000000413     C                   MOVEL     RAGUT         RSUT
055100000413     C                   MOVEL     REC80         CNCR80
055200000413      *
055300000413      * LA DATA DEL GIORNO LA PRENDO DA TIME
055400000413     C                   TIME                    W0140
055500000413     C                   MOVE      W0140         UDATE8
055600000414      * UDATE IN AAAAMMGG
055700000414     C     *eur          MOVEL     UDATE8        Wdata
055800000414     C     *iso          MOVEL     Wdata         UDATE8G
055900000413      *
056000041122     C**   KLBL          KLIST
056100041122     C**                 KFLD                    LBLAAP
056200041122     C**                 KFLD                    LBLLPP
056300041122     C**                 KFLD                    LBLNRP
056400041122     C**                 KFLD                    LBLNSP
056500041122     C**                 KFLD                    LBLAAN
056600041122     C**                 KFLD                    LBLLPN
056700041122     C**                 KFLD                    LBLNRN
056800041122     C**                 KFLD                    LBLNSN
056900000419      *
057000000419     C     KBLP          KLIST
057100000419     C                   KFLD                    BLPAAS
057200000419     C                   KFLD                    BLPLNP
057300000419     C                   KFLD                    BLPNRS
057400000419     C                   KFLD                    BLPNSP
057500020205      *
057600020205     C     KBL89         KLIST
057700020205     C                   KFLD                    blPAAS
057800020205     C                   KFLD                    blPLNP
057900020205     C                   KFLD                    blPNRS
058000020205     C                   KFLD                    blPNSP
058100060221     C                   KFLD                    ar4TRC
058200000426      *
058300000426     C     KAR6          KLIST
058400000426     C                   KFLD                    ar6AAS
058500000426     C                   KFLD                    ar6LNP
058600000426     C                   KFLD                    ar6NRS
058700000426     C                   KFLD                    ar6NSP
058800000426     C                   KFLD                    ar6TRC
058900000427      *
059000000427     C     KBLT          KLIST
059100000427     C                   KFLD                    bltFLS
059200000427     C                   KFLD                    bltLNA
059300000427     C                   KFLD                    bltNRS
059400000427     C                   KFLD                    bltNSC
059500060303     C     KTAB1         KLIST
059600060303     C                   KFLD                    CODUT             1 0
059700060303     C                   KFLD                    KCOD              2
059800060303     C                   KFLD                    KKEY              8
059900000413      *
060000000413     C                   ENDSR
060100000413      *---------------------------------------------------------------*
060200000419** SCADD
060300000419ADDPFM FILE(FNBLP00R  )                 MBR(SEDE)
060400060221ADDPFM FILE(FIAR400R  )                 MBR(SEDE)
060500030110ADDPFM FILE(FIAR500R  )                 MBR(SEDE)
060600000419ADDPFM FILE(FIAR600R  )                 MBR(SEDE)
060700000419ADDPFM FILE(FIAR700R  )                 MBR(SEDE)
060800051116ADDPFM FILE(FIAR900R  )                 MBR(SEDE)
060900000419ADDPFM FILE(FNBLT00R  )                 MBR(SEDE)
061000000413** SCALC
061100000413ALCOBJ OBJ((FNBLP00R *FILE *EXCL           SEDE))
061200060221ALCOBJ OBJ((FIAR400R *FILE *EXCL           SEDE))
061300030110ALCOBJ OBJ((FIAR500R *FILE *EXCL           SEDE))
061400000413ALCOBJ OBJ((FIAR600R *FILE *EXCL           SEDE))
061500000413ALCOBJ OBJ((FIAR700R *FILE *EXCL           SEDE))
061600051116ALCOBJ OBJ((FIAR900R *FILE *EXCL           SEDE))
061700000413ALCOBJ OBJ((FNBLT00R *FILE *EXCL           SEDE))
061800000413** SCOVR
061900000419OVRDBF FILE(FNBLP00R  )                 MBR(SEDE)
062000060221OVRDBF FILE(FIAR400R  )                 MBR(SEDE)
062100030110OVRDBF FILE(FIAR500R  )                 MBR(SEDE)
062200000419OVRDBF FILE(FIAR600R  )                 MBR(SEDE)
062300000419OVRDBF FILE(FIAR700R  )                 MBR(SEDE)
062400051116OVRDBF FILE(FIAR900R  )                 MBR(SEDE)
062500000419OVRDBF FILE(FNBLT00R  )                 MBR(SEDE)
