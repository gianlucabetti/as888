000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000413
000300000413      *--------------------------------------------------------------*
000400030110      *   RICEZIONE IN ARRIVO DELLE BOLLE RIGENERATE DA SEDE         *
000500000413      *--------------------------------------------------------------*
000600000413
000700000413      *****************************************************************
000800000413      *  RIEPILOGO INDICATORI
000900000413      *****************************************************************
001000030110      * 20    - DI COMODO
001100000419      * 31    - DI COMODO
001200000413      * 91    - FILE GIA' ALLOCATO
001300000413      *****************************************************************
001400000413
001500000419     FFNARB00R  UF   E           K DISK    USROPN RENAME(FNARB000:FNARBR)
001600060221     FFIAR400R  UF   E           K DISK    USROPN RENAME(FIAR4000:FIAR4R)
001700030110     FFIAR500R  Uf   e           k disk    usropn rename(FIAR5000:FIAR5R)
001800000413     FFIAR600R  UF   E           K DISK    USROPN RENAME(FIAR6000:FIAR6R)
001900000413     FFIAR700R  UF   E           K DISK    USROPN RENAME(FIAR7000:FIAR7R)
002000051116     FFiAR900R  UF   E           K DISK    USROPN RENAME(FiAR9000:FiAR9R)
002100000419     FFNART00R  UF   E           K DISK    USROPN RENAME(FNART000:FNARTR)
002200041122     F***FNLBL00R  UF   E           K DISK    USROPN RENAME(FNLBL000:FNLBLR)
002300040831     F***FNEVB00R  UF   E           K DISK    USROPN RENAME(FNEVB000:FNEVBR)
002400000413      *
002500021121     FFNARB01L  uF A E           K DISK    prefix(o)
002600060221     FFIAR401L  UF A E           K DISK
002700030110     FFIAR501L  Uf a e           k disk
002800000426     FFIAR601L  UF A E           K DISK    prefix(o)
002900991126     FFIAR701L  UF A E           K DISK
003000051116     FFiAR901L  UF A E           K DISK
003100000427     FFNART27L  UF A E           K DISK
003200060303     FTABEL00F  IF A E           K DISK
003300041122     F***FNLBL02L  UF A E           K DISK
003400040831     F***FNEVB01L  UF A E           K DISK
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
004500060303     D DS3A          E DS
004600000426     D DS_FIAR6      E DS                  extname(FIAR600F)
004700000426     D DS_FIAR6_O    E DS                  extname(FIAR600F) prefix(o)
004800021121     D DS_Fnarb      E DS                  extname(Fnarb00F)
004900021121     D DS_Fnarb_O    E DS                  extname(Fnarb00F) prefix(o)
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
006000000419     d RECARB          s              8  0   inz
006100030110     d RecAR5          s              8  0   inz
006200000419     d RECAR6          s              8  0   inz
006300000419     d RECAR7          s              8  0   inz
006400000419     d RECAR9          s              8  0   inz
006500000419     d RECART          s              8  0   inz
006600000419     d RECLBL          s              8  0   inz
006700040831     d*RECEVB          s              8  0   inz
006800000414     D Wdata           S               D   DATFMT(*eur)
006900000413      *--------------------------------------------------------------*
007000000413      *
007100000419      * Se il membro da trascodificare non è "SEDE" vado a fine programma
007200000419     C                   If        MBRFIL = 'SEDE'
007300000419      * Apertura membri
007400900526     C                   EXSR      CAEXA
007500000413      *
007600000419      * Proseguo se non ho avuto errori in apertura
007700000413    1C                   IF        *in91 = *off
007800000419      *
007900000419      * Elaborazione record bolla ricevuti
008000000419     C     *LOVAL        SETLL     fnarb00r
008100000419     C                   READ      fnarb00r
008200000419    2C                   dow       not %EOF(fnarb00r)
008300000419     C                   EXSR      aggbol
008400000419     C                   READ      fnarb00r
008500000419    2C                   enddo
008600000419      *
008700000419      * Elaborazione legami ricevuti
008800041122     C**   *LOVAL        SETLL     fnlbl00r
008900041122     C**                 READ      fnlbl00r
009000041122    2C**                 dow       not %EOF(fnlbl00r)
009100041122     C**                 EXSR      aggleg
009200041122     C**                 READ      fnlbl00r
009300041122    2C**                 enddo
009400000413      *
009500000414      * Chiusura membro trascodificato e aggiornamento archivi di log
009600000419     C                   EXSR      AGGcmric
009700000413      *
009800000413     C                   Endif
009900000414     C                   Endif
010000000413      *
010100000413     C                   EVAL      *INLR = *ON
010200000413      **-------------------------------------------------------------**
010300000413      **  AGGIORNAMENTO FILES BOLLE                                  **
010400000413      **-------------------------------------------------------------**
010500950622     C     AGGBOL        BEGSR
010600000413      *
010700000419     C                   ADD       1             RECARB
010800000413      *
010900021121     C     KARB          chain     FNARB000
011000000417      *
011100021121    0C                   IF        not %found(fnARB01l)
011200021121     c                   eval      ds_fnarb_o = ds_fnarb
011300000419      * FNARB00F
011400021121     C                   z-add     udate8g       oARBdt1
011500021121     C                   z-add     udate8g       oARBdt2
011600021121     c                   movel     'R'           oARBft1
011700021121     c                   movel     'R'           oARBft2
011800000419     C                   WRITE     fnARB000
011900000413      *
012000060221      * FIAR400F
012100060221     C     KARB          SETLL     FIAR400R
012200060221    0C                   IF        %EQUAL(fIAR400r)
012300000414      *
012400060221     C     KARB          DELETE    FIAR4000
012500060221     C                   DOW       %FOUND(fiAR401l)
012600060221     C     KARB          DELETE    FiAR4000
012700000414     C                   ENDDO
012800000414      *
012900060221     C     KARB          READE     FiAR400R
013000000413      *
013100060221    2C                   DOW       not %EOF(fiAR400r)
013200000419     C                   z-add     udate8g       AR4dtr
013300000419     c                   movel     'R'           AR4ftr
013400000502      * monitorizzo l'errore in quanto nel file di ricezione potrei avere i
013500000502      * record doppi (2 richieste di ripristino consecutive)
013600060221     C                   WRITE     FiAR4000                             31
013700060221     C                   DELETE    FiAR4R
013800060221     C     KARB          READE     FiAR400R
013900950622    2C                   ENDDO
014000000413      *
014100950622    1C                   ENDIF
014200030110      *
014300030110      * FIAR500F
014400030110     C     KARB          setll     FIAR500R
014500030110if  1C                   If        %EQUAL(FIAR500R)
014600030110      *
014700030110do  2C                   Dou       not %FOUND(FIAR501L)
014800030110     C     KARB          DELETE    FIAR5000
014900030110e   2C                   Enddo
015000030110      *
015100030110     C     KARB          reade     FIAR500R
015200030110do  2C                   Dow       not %EOF(FIAR500R)
015300030110     C                   add       1             RecAR5
015400030110     C                   movel     'R'           AR5ft1
015500030110     C                   z-add     udate8g       AR5dt1
015600030130     C                   movel     'R'           AR5ft2
015700030130     C                   z-add     udate8g       AR5dt2
015800040315     C                   WRITE     FIAR5000                             31
015900030110     C                   DELETE    FIAR5R
016000030110     C     KARB          reade     FIAR500R
016100030110    2C                   Enddo
016200030110      *
016300030110    1C                   Endif
016400000414      *
016500000414      * FIAR600F
016600000426     C     KARB          SETLL     FIAR600R
016700000426    0C                   IF        %EQUAL(fiar600r)
016800000426     C                   EXSR      AggTassaz
016900000426    1C                   ENDIF
017000000414      *
017100051116      * FIAR900F
017200051116     C     KARB          CHAIN     FiAR900R
017300051116    0C                   IF        %FOUND(fiar900r)
017400000426      *
017500051117     C     KARB          SETLL     FiAR901L
017600051116    0C                   if        not %EQUAL(fiar901l)
017700051116     C                   WRITE     FiAR9000
017800000426    1C                   endif
017900051116     C                   DELETE    FiAR9R
018000000426      *
018100000426    1C                   ENDIF
018200000426      *
018300000419      * FNART00F
018400000427     C     KARB          SETLL     FNART00R
018500000427    0C                   IF        %EQUAL(fnart00r)
018600000427     C                   EXSR      AggSegnac
018700000427     C                   ENDIF
018800000414      *
018900000414      * FNEVB00F
019000040831     C***  KARB          SETLL     FNEVB00R
019100040831    0C***                IF        %EQUAL(fnevb00r)
019200000414      *
019300040831     C***  KARB          READE     FNEVB00R
019400000414      *
019500040831    2C***                DOW       not %EOF(fnevb00r)
019600040831     C***                add       1             RECEVB
019700040831     C***                z-add     udate8g       EVBdtr
019800040831     c***                movel     'R'           EVBftr
019900040831     C***                WRITE     FNEVB000
020000040831     C***                DELETE    FNEVBR
020100040831     C***  KARB          READE     FNEVB00R
020200040831    2C***                ENDDO
020300000414      *
020400040831    1C***                ENDIF
020500000413      *
020600000419      * CANCELLO IL RECORD RICEVUTO DI FNARB00R
020700000419     C                   DELETE    FNARBR
020800000414      *
020900950630   X0C                   ELSE
021000020205     c* SE ESISTE LA BOLLA e
021100060221      * ricevo il record 9 nel file fiar400r sono nel caso di c/assegni
021200020204      * omessi
021300021121     c*
021400020205     c                   clear                   flgcao            1
021500060221     c     karb          setll     fiar400r
021600020205     c                   do        *hival
021700060221     c     karb          reade     fiar400r
021800060221    0C                   if        %Eof(fiar400r)
021900020205     c                   leave
022000020205     c                   end
022100020212    0C                   if        ar4trc = '9' and
022200020212    0C                             %subst(ar4not: 1: 25)='PER C/ASSEGNO OMESSO'
022300020205     c                   movel     '1'           flgcao
022400020205     c                   leave
022500020205     c                   end
022600020205     c                   enddo
022700060303     c*
022800060303    1c                   if        flgcao=' '
022900060303     C                   EVAL      AR4TRC='9'
023000060303     c     kAR89         CHAIN(N)  fiar401l
023100060303    2C                   if        %FOUND (fiar401l)  AND
023200060303     C                             %subst(ar4not: 1: 25)='PER C/ASSEGNO OMESSO'
023300060303     c                   movel     '1'           flgcao
023400060303    3c                   end
023500060303    1c                   endif
023600060303     c*
023700020205     c*
023800020205     c                   if        flgcao = '1'
023900020205     C                   EXSR      srcass
024000020205     c                   end
024100020204      * Cancello i record se la bolla è già presente in FNARB00F
024200000417     C                   EXSR      DELrcd
024300000417      *
024400020204    0C                   ENDIF
024500000417      *
024600900530     C                   ENDSR
024700000426      **-------------------------------------------------------------**
024800000426      **  AGGIORNAMENTO FILE TASSAZIONE                              **
024900000426      **-------------------------------------------------------------**
025000000426     C     AGGTASSAZ     BEGSR
025100000426      *
025200000426     c                   clear                   DS_fiar6_O
025300000426     c                   clear                   DS_fiar6
025400000426     C     KARB          READE     FIAR600R
025500000426      *
025600000426    2C                   DOW       not %EOF(fiar600r)
025700000426      *
025800000426      *  Aggancio con il tipo rcd il file effettivo
025900000426     C     KAR6          CHAIN     FIAR601L
026000000426      *
026100000426      *  Se lo trovo e non ha tassazione mentre quello ricevuto si allora
026200000426      *   cancello i record effettivi
026300000426     c                   IF        %FOUND(fiar601l)   and
026400000426     c                             oAR6div = *blanks  and  AR6div <> *blanks
026500000426     C                   DELETE    FIAR601L
026600000426     C     KAR6          DELETE    FIAR701L
026700000426    2C                   DOW       %FOUND(fiar701L)
026800000426     C     KAR6          DELETE    FIAR701L
026900000426    2C                   ENDDO
027000000426     C                   ENDIF
027100000426      *
027200000426      *  Se LO TROVO e non ha tassazione mentre quello ricevuto si  OPPURE
027300000426      *  NON LO TROVO scrivo FIAR6 e FIAR7 del tipo rcd in esame
027400000426     c                   IF        (%FOUND(fiar601l) and
027500000426     c                             oAR6div = *blanks and AR6div <> *blanks)
027600000426     c                               OR  NOT %FOUND(fiar601l)
027700000426     c                   EVAL      DS_fiar6_O = DS_fiar6
027800000426     C                   WRITE     FIAR6000
027900000426      *
028000000426     C     KAR6          SETLL     FIAR700R
028100000426     C     KAR6          READE     FIAR700R
028200000426    2C                   DOW       not %EOF(fiar700r)
028300000426     C                   ADD       1             RECAR7
028400000608     C                   WRITE     FIAR7000                             31
028500000426     C                   DELETE    FIAR7R
028600000426     C     KAR6          READE     FIAR700R
028700000426    2C                   ENDDO
028800000426     c                   ENDIF
028900000426      *
029000000426      *   Cancello FIAR6 ricevuto
029100000426     C                   add       1             RECAR6
029200000426     C                   DELETE    FIAR6R
029300000426      *
029400000426      *   Cancello FIAR7 ricevuti se non già fatto
029500000426     C     KAR6          delete    FIAR700R
029600000426    2C                   DOW       %FOUND(fiar700r)
029700000426     C                   add       1             RECAR7
029800000426     C     KAR6          delete    FIAR7R
029900000426    2C                   ENDDO
030000000426      *
030100000426     c                   clear                   DS_fiar6_O
030200000426     c                   clear                   DS_fiar6
030300000426     C     KARB          READE     FIAR600R
030400000426     c                   ENDDO
030500000426      *
030600000426     C                   ENDSR
030700000427      **-------------------------------------------------------------**
030800000427      **  AGGIORNAMENTO FILE SEGNACOLLI                              **
030900000427      **-------------------------------------------------------------**
031000000427     C     AGGSEGNAC     BEGSR
031100000427      *
031200000427     c                   clear                   Wsegna
031300000427      * Verifico se tutti i segnacolli della spedizione sono univoci
031400000427     C     KARB          READE     FNART00R
031500000427      *
031600000427    2C                   DOW       not %EOF(fnart00r)
031700000427     C     kart          SETLL     FNART000
031800000427     C                   IF        %equal(fnart27l)
031900000427     c                   EVAL      Wsegna = 'S'
032000000427     C                   LEAVE
032100000427     C                   ENDIF
032200000427     C     KARB          READE     FNART00R
032300000427    2C                   ENDDO
032400000427      *
032500000427      * Se i segnacolli non sono univoci cancello i record ricevuti
032600000427     C                   IF        WSEGNA = 'S'
032700000427      *
032800000427     C     KARB          DELETE    FNARTR
032900000427     C                   DOW       %FOUND(fnart00r)
033000000427     C                   add       1             RECART
033100000427     C     KARB          DELETE    FNARTR
033200000427     C                   ENDDO
033300000427      *
033400000427      * Se i segnacolli sono univoci aggiorno il file effettivo
033500000427     C                   ELSE
033600000427      *
033700000427     C     KARB          SETLL     FNART00R
033800000427     C     KARB          READE     FNART00R
033900000427      *
034000000427    2C                   DOW       not %EOF(fnart00r)
034100000427     C                   add       1             RECART
034200000427     C                   z-add     udate8g       ARTdtr
034300000427     c                   movel     'R'           ARTftr
034400000608     C                   WRITE     FNART000                             31
034500000427     C                   DELETE    FNARTR
034600000427     C     KARB          READE     FNART00R
034700000427    2C                   ENDDO
034800000427      *
034900000427     C                   ENDIF
035000000427      *
035100000427     C                   ENDSR
035200000419      **-------------------------------------------------------------**
035300000419      **  AGGIORNAMENTO FILE LEGAMI                                  **
035400000419      **-------------------------------------------------------------**
035500041122     C**   AGGLEG        BEGSR
035600000419      *
035700000419      *
035800041122     C**                 ADD       1             RECLBL
035900041122     C**   KLBL          SETLL     FNLBL02L
036000000419      *
036100041122    0C**                 IF        not %EQUAL(fnlbl02L)
036200041122     C**                 WRITE     FNLBL000
036300041122    1C**                 ENDIF
036400000419      *
036500041122     C**                 DELETE    FNLBLR
036600000419      *
036700041122     C**                 ENDSR
036800020204      **-------------------------------------------------------------**
036900020204      **  AGGIORNAMENTO FILES x C/ASSEGNI OMESSI                     **
037000020204      **-------------------------------------------------------------**
037100020204     C     srcass        BEGSR
037200060221      * FiAR400F solo record 8 e 9
037300060221     C     KARb          SETLL     FiAR400R
037400020204     c                   do        *hival
037500060221     C     KARB          READE     FiAR400R
037600060221    0C                   IF        %Eof(fiAR400r)
037700020204     c                   leave
037800020204     c                   end
037900020204    0C                   IF        ar4trc <> '8' and ar4trc <> '9'
038000020204     c                   iter
038100020204     c                   end
038200060221     C     KAR89         DELETE    FiAR4000
038300020204     C                   z-add     udate8g       AR4dtr
038400020204     c                   movel     'R'           AR4ftr
038500020204      * monitorizzo l'errore in quanto nel file di ricezione potrei avere i
038600020204      * record doppi (2 richieste di ripristino consecutive)
038700060221     C                   WRITE     FiAR4000                             31
038800020204    2C                   ENDDO
038900051116      * FiAR900F
039000051116     C     KARB          CHAIN     FiAR900R
039100051116    0C                   IF        %FOUND(fiar900r)
039200051116     C     KARB          SETLL     FiAR901L
039300051116    0C                   if        not %EQUAL(fiar901l)
039400051116     C                   WRITE     FiAR9000
039500020204    1C                   endif
039600020204    1C                   ENDIF
039700021121      * FNARB00F
039800060303     c*Aggiorno Oarbcbo, solo se il codice bolla non prevede C/A
039900060303     C                   MOVEL     '3A'          KCOD
040000060303     c                   movel(P)  oarbcbo       kkey
040100060303     C     KTAB1         CHAIN     TABEL00F                           30
040200060303     C  N30              MOVEL     TBLUNI        DS3a
040300060303     c   30              clear                   ds3a
040400060303     c                   if        §3afca=0
040500021121     c                   movel     arbicc        oARBicc
040600080901     c                   if        oarbica='S' and oarbnmi <> 0
040700060303     c                   movel     arbica        oARBica
040800060303     c                   endif
040900021121     c                   movel     arbcbo        oARBcbo
041000021121     C                   update    fnARB000
041100060303     c                   else
041200060303     c                   unlock    fnarb01l
041300060303     c                   endif
041400020204      *
041500020204     C                   ENDSR
041600000417      **-------------------------------------------------------------**
041700000417      **  CANCELLAZIONE BOLLE RICEVUTE                               **
041800000417      **-------------------------------------------------------------**
041900000417     C     DELRCD        BEGSR
042000000417      *
042100060221      * FiAR400F
042200060221     C     KARB          DELETE    FiAR4R
042300060221     C                   DOW       %FOUND(fiAR400r)
042400060221     C     KARB          DELETE    FiAR4R
042500000417     C                   ENDDO
042600030110      *
042700030110      * FIAR500F
042800030110     C     KARB          DELETE    FIAR5R
042900030110     C                   Dow       %FOUND(FIAR500R)
043000030110     C                   add       1             RecAR5
043100030110     C     KARB          DELETE    FIAR5R
043200030110     C                   Enddo
043300000417      *
043400000417      * FIAR600F
043500000419     C     KARB          DELETE    FIAR6R
043600000419     C                   DOW       %FOUND(fiar600r)
043700000417     C                   add       1             RECAR6
043800000419     C     KARB          DELETE    FIAR6R
043900000417     C                   ENDDO
044000000417      *
044100000417      * FIAR700F
044200000419     C     KARB          DELETE    FIAR7R
044300000419     C                   DOW       %FOUND(fiar700r)
044400000419     C                   add       1             RECAR7
044500000419     C     KARB          DELETE    FIAR7R
044600000417     C                   ENDDO
044700000417      *
044800051116      * FiAR900F
044900051116     C     KARB          DELETE    FiAR9R
045000051116     C                   DOW       %FOUND(fiAR900r)
045100051116     C     KARB          DELETE    FiAR9R
045200000417     C                   ENDDO
045300000417      *
045400000419      * FNART00F
045500000419     C     KARB          DELETE    FNARTR
045600000419     C                   DOW       %FOUND(fnART00r)
045700000419     C                   add       1             RECART
045800000419     C     KARB          DELETE    FNARTR
045900000417     C                   ENDDO
046000000417      *
046100000417      * FNEVB00F
046200040831     C***  KARB          DELETE    FNEVBR
046300040831     C***                DOW       %FOUND(fnevb00r)
046400040831     C***                add       1             RECEVB
046500040831     C***  KARB          DELETE    FNEVBR
046600040831     C***                ENDDO
046700000417      *
046800000419      * FNARB00F
046900000419     C                   DELETE    FNARBR
047000000417      *
047100000417     C                   ENDSR
047200000413      **-------------------------------------------------------------**
047300000413      **  ESEGUE APERTURA SUI MEMBRI                                 **
047400000413      **-------------------------------------------------------------**
047500900526     C     CAEXA         BEGSR
047600000413      *
047700000413     c                   Do        Wnumele       XX
047800000419      *
047900000419      * Aggiungo il membro
048000000419     C                   MOVEL(P)  ScAdd(xx)     comman
048100000419     C                   CALL      'QCMDEXC'                            31
048200000419     C                   PARM                    comman
048300000419     C                   PARM                    lung
048400000419      *
048500000419      * Alloco il membro, se non riesco chiudo il PGM
048600000413     C                   MOVEL(P)  ScAlc(xx)     comman
048700000413     C                   CALL      'QCMDEXC'                            91
048800000413     C                   PARM                    comman
048900000413     C                   PARM                    lung
049000000413      *
049100000413     C   91              LEAVE
049200000413      *
049300000419      * Eseguo l'ovrdbf, se non riesco chiudo il pgm
049400000413     C                   MOVEL(P)  ScOvr(xx)     comman
049500000419     C                   CALL      'QCMDEXC'                            91
049600000413     C                   PARM                    comman
049700000413     C                   PARM                    lung
049800000413      *
049900000419     C   91              LEAVE
050000000419      *
050100000419     C                   ENDDO
050200000419      *
050300000413      * Apro i file
050400000419     C                   IF        *IN91 = *off
050500000419     C                   open      FNARB00R
050600060221     C                   open      FiAR400R
050700030110     C                   open      FIAR500R
050800000413     C                   open      FIAR600R
050900000413     C                   open      FIAR700R
051000051116     C                   open      FiAR900R
051100000419     C                   open      FNART00R
051200041122     C***                open      FNLBL00R
051300040831     C****               open      FNEVB00R
051400000419     C                   ENDIF
051500000413      *
051600000413     C                   ENDSR
051700000419      *---------------------------------------------------------------*
051800000419      * Chiusura membro trascodificato e aggiornamento archivi di log
051900000419      *---------------------------------------------------------------*
052000000419     C     AGGcmric      BEGSR
052100000419      *
052200000419     C                   close     FNARB00R
052300060221     C                   close     FiAR400R
052400030110     C                   close     FIAR500R
052500000419     C                   close     FIAR600R
052600000419     C                   close     FIAR700R
052700051116     C                   close     FiAR900R
052800000419     C                   close     FNART00R
052900041122     C***                close     FNLBL00R
053000040831     C****               close     FNEVB00R
053100000419      *
053200000419     C                   ENDSR
053300000413      *---------------------------------------------------------------*
053400000413      * SUBROUTINE INIZIALE
053500000413      *---------------------------------------------------------------*
053600000413     C     *INZSR        BEGSR
053700000413      *
053800000413     C     *ENTRY        PLIST
053900000413     C                   PARM                    KPJBA
054000000413      *
054100000413     c                   EVAL      Wnumele = %elem(ScAlc)
054200000413      *
054300000413     C                   Z-ADD     1             CODUT
054400000413     C                   CALL      'X§PARUT'
054500000413     C                   PARM                    UT§DSE0F
054600000413     C                   MOVEL     RAGUT         RSUT
054700000413     C                   MOVEL     REC80         CNCR80
054800000413      *
054900000413      * LA DATA DEL GIORNO LA PRENDO DA TIME
055000000413     C                   TIME                    W0140
055100000413     C                   MOVE      W0140         UDATE8
055200000414      * UDATE IN AAAAMMGG
055300000414     C     *eur          MOVEL     UDATE8        Wdata
055400000414     C     *iso          MOVEL     Wdata         UDATE8G
055500000419      *
055600041122     C**   KLBL          KLIST
055700041122     C**                 KFLD                    LBLAAP
055800041122     C**                 KFLD                    LBLLPP
055900041122     C**                 KFLD                    LBLNRP
056000041122     C*+                 KFLD                    LBLNSP
056100041122     C**                 KFLD                    LBLAAN
056200041122     C**                 KFLD                    LBLLPN
056300041122     C**                 KFLD                    LBLNRN
056400041122     C**                 KFLD                    LBLNSN
056500000413      *
056600000419     C     KARB          KLIST
056700000419     C                   KFLD                    ARBAAS
056800000419     C                   KFLD                    ARBLNP
056900000419     C                   KFLD                    ARBNRS
057000000419     C                   KFLD                    ARBNSP
057100020204      *
057200020204     C     KAR89         KLIST
057300020204     C                   KFLD                    ARBAAS
057400020204     C                   KFLD                    ARBLNP
057500020204     C                   KFLD                    ARBNRS
057600020204     C                   KFLD                    ARBNSP
057700020205     C                   KFLD                    ar4trc
057800000426      *
057900000426     C     KAR6          KLIST
058000000426     C                   KFLD                    ar6AAS
058100000426     C                   KFLD                    ar6LNP
058200000426     C                   KFLD                    ar6NRS
058300000426     C                   KFLD                    ar6NSP
058400000426     C                   KFLD                    ar6TRC
058500000427      *
058600000427     C     KART          KLIST
058700000427     C                   KFLD                    artFLS
058800000427     C                   KFLD                    artLNA
058900000427     C                   KFLD                    artNRS
059000000427     C                   KFLD                    artNSC
059100060303     C     KTAB1         KLIST
059200060303     C                   KFLD                    CODUT             1 0
059300060303     C                   KFLD                    KCOD              2
059400060303     C                   KFLD                    KKEY              8
059500000413      *
059600000413     C                   ENDSR
059700000413      *---------------------------------------------------------------*
059800000419** SCADD
059900000419ADDPFM FILE(FNARB00R  )                 MBR(SEDE)
060000060221ADDPFM FILE(FIAR400R  )                 MBR(SEDE)
060100030110ADDPFM FILE(FIAR500R  )                 MBR(SEDE)
060200000419ADDPFM FILE(FIAR600R  )                 MBR(SEDE)
060300000419ADDPFM FILE(FIAR700R  )                 MBR(SEDE)
060400051116ADDPFM FILE(FIAR900R  )                 MBR(SEDE)
060500000419ADDPFM FILE(FNART00R  )                 MBR(SEDE)
060600000413** SCALC
060700000419ALCOBJ OBJ((FNARB00R *FILE *EXCL           SEDE))
060800060221ALCOBJ OBJ((FIAR400R *FILE *EXCL           SEDE))
060900030110ALCOBJ OBJ((FIAR500R *FILE *EXCL           SEDE))
061000000413ALCOBJ OBJ((FIAR600R *FILE *EXCL           SEDE))
061100000413ALCOBJ OBJ((FIAR700R *FILE *EXCL           SEDE))
061200051116ALCOBJ OBJ((FIAR900R *FILE *EXCL           SEDE))
061300000419ALCOBJ OBJ((FNART00R *FILE *EXCL           SEDE))
061400000413** SCOVR
061500000419OVRDBF FILE(FNARB00R  )                 MBR(SEDE)
061600060221OVRDBF FILE(FIAR400R  )                 MBR(SEDE)
061700030110OVRDBF FILE(FIAR500R  )                 MBR(SEDE)
061800000419OVRDBF FILE(FIAR600R  )                 MBR(SEDE)
061900000419OVRDBF FILE(FIAR700R  )                 MBR(SEDE)
062000051116OVRDBF FILE(FIAR900R  )                 MBR(SEDE)
062100000419OVRDBF FILE(FNART00R  )                 MBR(SEDE)
