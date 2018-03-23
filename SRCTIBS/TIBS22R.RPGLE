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
003100121106     FFIART27L  UF A E           K DISK
003200060303     Ftabel00f  IF A E           K DISK
003300041122     F****FNLBL02L  UF A E           K DISK
003400040831     F****FNEVB01L  UF A E           K DISK
003500050909     fTigcp51l  uf   e           k disk
003501171121     ffipnd01l  uf   e           k disk
003600121106     fazorg01l  if   e           k disk
003700121106     ffnevb04l  if   e           k Disk
003800000413      *
003900041122     D SCadd           S             55    DIM(7) CTDATA PERRCD(1)
004000041122     D SCalc           S             55    DIM(7) CTDATA PERRCD(1)
004100041122     D SCovr           S             55    DIM(7) CTDATA PERRCD(1)
004200000413      *
004300000000     D KPJBA         E DS
004400900526     D  LEGFIL               483    492
004500900526     D  MBRFIL               493    502
004600000413     D UT§DSE0F      E DS
004700991126     D CNCR80        E DS
004800060303     D ds3a          E DS
004900121106     D og143         E DS
005000121106     d dar5gen       e ds
005100000426     D DS_FIAR6      E DS                  extname(FIAR600F)
005200000426     D DS_FIAR6_O    E DS                  extname(FIAR600F) prefix(o)
005300021121     D DS_Fnblp      E DS                  extname(Fnblp00F)
005400021121     D DS_Fnblp_O    E DS                  extname(Fnblp00F) prefix(o)
005500000413      *
005600000414     D XX              S              3  0
005700000414     D lung            S             15  5 INZ(55)
005800000413     D comman          S             80
005900000413     D Wnumele         S              5P 0
006000000413     D RSUT            S             20
006100000427     D Wsegna          S              1
006200000413     D W0140           S             14  0
006300000413     D UDATE8          S              8  0
006400000414     D UDATE8G         S              8  0
006500000419     d RECBLP          s              8  0   inz
006600030110     d RecAR5          s              8  0   inz
006700000419     d RECAR6          s              8  0   inz
006800000419     d RECAR7          s              8  0   inz
006900000419     d RECBLT          s              8  0   inz
007000000414     D Wdata           S               D   DATFMT(*eur)
007100121106     d ktrd            s                   like(ar5trd) inz('GEN')
007200121106     d kcev            s                   like(evbcev) inz('P  ')
007300000413      *--------------------------------------------------------------*
007400000413      *
007500000419      * Se il membro da trascodificare non è "SEDE" vado a fine programma
007600000419     C                   If        MBRFIL = 'SEDE'
007700000419      * Apertura membri
007800900526     C                   EXSR      CAEXA
007900000413      *
008000000419      * Proseguo se non ho avuto errori in apertura
008100000413    1C                   IF        *in91 = *off
008200000414      *
008300000419      * Elaborazione record bolla ricevuti
008400000413     C     *LOVAL        SETLL     fnblp00r
008500000413     C                   READ      fnblp00r
008600000419    2C                   dow       not %EOF(fnblp00r)
008700000413     C                   EXSR      aggbol
008800000413     C                   READ      fnblp00r
008900000419    2C                   enddo
009000000419      *
009100000419      * Elaborazione legami ricevuti
009200041122     C**   *LOVAL        SETLL     fnlbl00r
009300041122     C**                 READ      fnlbl00r
009400041122    2C**                 dow       not %EOF(fnlbl00r)
009500041122     C**                 EXSR      aggleg
009600041122     C**                 READ      fnlbl00r
009700041122    2C**                 enddo
009800000413      *
009900000419      * Chiusura membri e aggiornamento archivi di log
010000041122     C**                 EXSR      AGGcmric
010100000413      *
010200000413     C                   Endif
010300000414     C                   Endif
010400000413      *
010500000413     C                   EVAL      *INLR = *ON
010600000413      **-------------------------------------------------------------**
010700000413      **  AGGIORNAMENTO FILES BOLLE                                  **
010800000413      **-------------------------------------------------------------**
010900950622     C     AGGBOL        BEGSR
011000000413      *
011100950623     C                   ADD       1             RECBLP
011200000413      *
011300021121     C     KBLP          chain     FNBLP000
011400000417      *
011500021121    0C                   IF        not %found(fnblp01l)
011600021121     c                   eval      ds_fnblp_o = ds_fnblp
011700000417      * FNBLP00F
011800021121     C                   z-add     udate8g       oBLPdt1
011900021121     C                   z-add     udate8g       oBLPdt2
012000021121     c                   movel     'R'           oBLPft1
012100021121     c                   movel     'R'           oBLPft2
012200000413     C                   WRITE     fnblp000
012300000413      *
012400060221      * FIAR400F
012500060221     C     KBLP          SETLL     FIAR400R
012600060221    1C                   IF        %EQUAL(fiar400r)
012700000414      *
012800060221     C     KBLP          DELETE    Fiar4000
012900060221     C                   DOW       %FOUND(fiar401l)
013000060221     C     KBLP          DELETE    Fiar4000
013100000414     C                   ENDDO
013200000414      *
013300060221     C     KBLP          READE     Fiar400R
013400000413      *
013500060221    2C                   DOW       not %EOF(fiar400r)
013600060221     C                   z-add     udate8g       ar4dtr
013700060221     c                   movel     'R'           ar4ftr
013800000502      * monitorizzo l'errore in quanto nel file di ricezione potrei avere i
013900000502      * record doppi (2 richieste di ripristino consecutive)
014000060221     C                   WRITE     Fiar4000                             31
014100060221     C                   DELETE    Fiar4R
014200060221     C     KBLP          READE     Fiar400R
014300950622    2C                   ENDDO
014400000413      *
014500950622    1C                   ENDIF
014600030110      *
014700030110      * FIAR500F
014800030110     C     KBLP          setll     FIAR500R
014900030110if  1C                   If        %EQUAL(FIAR500R)
015000030110      *
015100030110do  2C                   Dou       not %FOUND(FIAR501L)
015200030110     C     KBLP          DELETE    FIAR5000
015300030110e   2C                   Enddo
015400030110      *
015500030110     C     KBLP          reade     FIAR500R
015600030110do  2C                   Dow       not %EOF(FIAR500R)
015700030110     C                   add       1             RecAR5
015800030110     C                   movel     'R'           AR5ft1
015900030110     C                   z-add     udate8g       AR5dt1
016000030130     C                   movel     'R'           AR5ft2
016100030130     C                   z-add     udate8g       AR5dt2
016200040112     C                   WRITE     FIAR5000                             31
016300030110     C                   DELETE    FIAR5R
016400030110     C     KBLP          reade     FIAR500R
016500030110    2C                   Enddo
016600030110      *
016700030110    1C                   Endif
016800000414      *
016900000414      * FIAR600F
017000000426     C     KBLP          SETLL     FIAR600R
017100000426    0C                   IF        %EQUAL(fiar600r)
017200000426     C                   EXSR      AggTassaz
017300000426    1C                   ENDIF
017400000414      *
017500051116      * Fiar900F
017600051116     C     KBLP          CHAIN     Fiar900R
017700051116    0C                   IF        %FOUND(fiar900r)
017800000426      *
017900051116     C     KBLP          SETLL     Fiar901L
018000051116    0C                   if        not %EQUAL(fiar901l)
018100051116     C                   WRITE     Fiar9000
018200000426    1C                   endif
018300051116     C                   DELETE    Fiar9R
018400000414      *
018500000414    1C                   ENDIF
018600000414      *
018700000414      * FNBLT00F
018800000414     C     KBLP          SETLL     FNBLT00R
018900000414    0C                   IF        %EQUAL(fnblt00r)
019000000427     C                   EXSR      AggSegnac
019100000427     C                   ENDIF
019200000414      *
019300000414      * FNEVB00F
019400040831     C***  KBLP          SETLL     FNEVB00R
019500040831    0C****               IF        %EQUAL(fnevb00r)
019600000414      *
019700040831     C***  KBLP          READE     FNEVB00R
019800000414      *
019900040831    2C****               DOW       not %EOF(fnevb00r)
020000040831     C****               add       1             RECEVB
020100040831     C****               z-add     udate8g       EVBdtr
020200040831     c****               movel     'R'           EVBftr
020300040831     C****               WRITE     FNEVB000
020400040831     C****               DELETE    FNEVBR
020500040831     C**** KBLP          READE     FNEVB00R
020600040831    2C****               ENDDO
020700000414      *
020800040831    1C****               ENDIF
020900050909
021000050909      * Controllo se la bolla che ho appena rigenerato ha giacenza fleggata
021100050909      * solo per la sede in questo caso è da sfleggare
021200050909     c     kblp          Setll     Tigcp51l
021300050909     c                   Do        *Hival
021400050909     c     kblp          Reade     Tigcp51l
021500050909     c                   If        %Eof(Tigcp51l)
021600050909     c                   Leave
021700050909     c                   EndIf
021800050909     c                   If        gcpsede = 'S'
021900050909     c                   Clear                   gcpsede
022000050909     c                   Update    tigcp000
022100050909     c                   EndIf
022200050909     c                   EndDo
022201171121      * Controllo se la bolla che ho appena rigenerato ha record su fipnd
022202171121      * è da sflaggare in quanto non più in gestione solo a sede
022203171121     c     kblp          chain     fipnd01l
022206171121     c                   If        %found(fipnd01l)
022210171121     c                   Clear                   pnddgs
022211171121     c                   Update    fipnd000
022213171121     c                   endif
022300000413      *
022400000419      * CANCELLO IL RECORD RICEVUTO DI FNBLP00R
022500950630     C                   DELETE    FNBLPR
022600000414      *
022700950630   X0C                   ELSE
022800020205     c* SE ESISTE LA BOLLA e
022900060221      * ricevo il record 9 nel file fiar400r sono nel caso di c/assegni
023000020205      * omessi
023100020205     c                   clear                   flgcao            1
023200060221     c     kblp          setll     fiar400r
023300060303    1c                   do        *hival
023400060221     c     kblp          reade     fiar400r
023500060303    2C                   if        %Eof(fiar400r)
023600020205     c                   leave
023700060303    2c                   end
023800060303    2C                   if        ar4trc = '9' and
023900060303     C                             %subst(ar4not: 1: 25)='PER C/ASSEGNO OMESSO'
024000020205     c                   movel     '1'           flgcao
024100020205     c                   leave
024200060303    2c                   end
024300060303    1c                   enddo
024400060303     c*
024500060303    1c                   if        flgcao=' '
024600060303     C                   EVAL      AR4TRC='9'
024700060303     c     kbl89         CHAIN(N)  fiar401l
024800060303    2C                   if        %FOUND (fiar401l) AND
024900060303     C                             %subst(ar4not: 1: 25)='PER C/ASSEGNO OMESSO'
025000060303     c                   movel     '1'           flgcao
025100060303    2c                   endif
025200060303    1c                   endif
025300020205     c*
025400020205     c                   if        flgcao = '1'
025500020205     C                   EXSR      srcass
025600020205     c                   end
025700000419      * Cancello i record se la bolla è già presente in FNBLP00F
025800000417     C                   EXSR      DELrcd
025900000417      *
026000950630    0C                   ENDIF
026100000417      *
026200900530     C                   ENDSR
026300000426      **-------------------------------------------------------------**
026400000426      **  AGGIORNAMENTO FILE TASSAZIONE                              **
026500000426      **-------------------------------------------------------------**
026600000426     C     AGGTASSAZ     BEGSR
026700000426      *
026800000426     c                   clear                   DS_fiar6_O
026900000426     c                   clear                   DS_fiar6
027000000426     C     KBLP          READE     FIAR600R
027100000426      *
027200000426    2C                   DOW       not %EOF(fiar600r)
027300000426      *
027400000426      *  Aggancio con il tipo rcd il file effettivo
027500000426     C     KAR6          CHAIN     FIAR601L
027600000426      *
027700000426      *  Se lo trovo e non ha tassazione mentre quello ricevuto si allora
027800000426      *   cancello i record effettivi
027900000426     c                   IF        %FOUND(fiar601l)   and
028000000426     c                             oAR6div = *blanks  and  AR6div <> *blanks
028100000426     C                   DELETE    FIAR601L
028200000426     C     KAR6          DELETE    FIAR701L
028300000426    2C                   DOW       %FOUND(fiar701L)
028400000426     C     KAR6          DELETE    FIAR701L
028500000426    2C                   ENDDO
028600000426     C                   ENDIF
028700000426      *
028800000426      *  Se LO TROVO e non ha tassazione mentre quello ricevuto si  OPPURE
028900000426      *  NON LO TROVO scrivo FIAR6 e FIAR7 del tipo rcd in esame
029000000426     c                   IF        (%FOUND(fiar601l) and
029100000426     c                             oAR6div = *blanks and AR6div <> *blanks)
029200000426     c                               OR  NOT %FOUND(fiar601l)
029300000426     c                   EVAL      DS_fiar6_O = DS_fiar6
029400000502     C                   WRITE     FIAR6000
029500000426      *
029600000426     C     KAR6          SETLL     FIAR700R
029700000426     C     KAR6          READE     FIAR700R
029800000426    2C                   DOW       not %EOF(fiar700r)
029900000426     C                   ADD       1             RECAR7
030000000608     C                   WRITE     FIAR7000                             31
030100000426     C                   DELETE    FIAR7R
030200000426     C     KAR6          READE     FIAR700R
030300000426    2C                   ENDDO
030400000426     c                   ENDIF
030500000426      *
030600000426      *   Cancello FIAR6 ricevuto
030700000426     C                   add       1             RECAR6
030800000426     C                   DELETE    FIAR6R
030900000426      *
031000000426      *   Cancello FIAR7 ricevuti se non già fatto
031100000426     C     KAR6          delete    FIAR700R
031200000426    2C                   DOW       %FOUND(fiar700r)
031300000426     C                   add       1             RECAR7
031400000426     C     KAR6          delete    FIAR7R
031500000426    2C                   ENDDO
031600000426      *
031700000426     c                   clear                   DS_fiar6_O
031800000426     c                   clear                   DS_fiar6
031900000426     C     KBLP          READE     FIAR600R
032000000426     c                   ENDDO
032100000426      *
032200000426     C                   ENDSR
032300000427      **-------------------------------------------------------------**
032400000427      **  AGGIORNAMENTO FILE SEGNACOLLI                              **
032500000427      **-------------------------------------------------------------**
032600000427     C     AGGSEGNAC     BEGSR
032700000427      *
032800000427     c                   clear                   Wsegna
032900000427      * Verifico se tutti i segnacolli della spedizione sono univoci
033000000427     C     KBLP          READE     FNBLT00R
033100000427      *
033200000427    2C                   DOW       not %EOF(fnblt00r)
033300000427     C     Kblt          SETLL     FNBLT000
033400000427     C                   IF        %equal(fnblt27l)
033500000427     c                   EVAL      Wsegna = 'S'
033600000427     C                   LEAVE
033700000427     C                   ENDIF
033800000427     C     KBLP          READE     FNBLT00R
033900000427    2C                   ENDDO
034000000427      *
034100000427      * Se i segnacolli non sono univoci cancello i record ricevuti
034200000427     C                   IF        WSEGNA = 'S'
034300000427      *
034400000427     C     KBLP          DELETE    FNBLTR
034500000427     C                   DOW       %FOUND(fnblt00r)
034600000427     C                   add       1             RECBLT
034700000427     C     KBLP          DELETE    FNBLTR
034800000427     C                   ENDDO
034900000427      *
035000000427      * Se i segnacolli sono univoci aggiorno il file effettivo
035100000427     C                   ELSE
035200000427      *
035300000427     C     KBLP          SETLL     FNBLT00R
035400000427     C     KBLP          READE     FNBLT00R
035500000427      *
035600000427    2C                   DOW       not %EOF(fnblt00r)
035700000427     C                   add       1             RECBLT
035800000427     C                   z-add     udate8g       BLTdtr
035900000427     c                   movel     'R'           BLTftr
036000000608     C                   WRITE     FNBLT000                             31
036100000427     C                   DELETE    FNBLTR
036200121106     c* Scrivo FIART unico
036300121106     c                   eval      artaas=bltaas
036400121106     c                   eval      artlnp=bltlnp
036500121106     c                   eval      artnrs=bltnrs
036600121106     c                   eval      artnsp=bltnsp
036700121106     c                   eval      artlna=bltlna
036800121106     c                   eval      artfls=bltfls
036900121106     c                   eval      artnsc=bltnsc
037000121106     c                   eval      artvuc=bltvuc
037100121106     c                   eval      artfvc=bltfvc
037200121106     c                   eval      artpuc=bltpuc
037300121106     c                   eval      artfpc=bltfpc
037400121106     c                   eval      artdfv=bltdfv
037500121106     c                   eval      artnpp=bltnpp
037600121106     c                   eval      artnfv=bltnfv
037700121106     c                   eval      artdse=bltdse
037800121106     c                   eval      artnpe=bltnpe
037900121106     c                   eval      artdet=bltdet
038000121106     c                   eval      artdam=bltdam
038100121107     c                   eval      artnap=86
038200121106    3c                   if        artlna=blptfa  and artdam>0
038300121106     c                   eval      artdamf=bltdam
038400121107     c                   eval      artnapf=artnap
038500121106     c                   endif
038600121106     c
038700121106     c                   eval      artdut=bltdut
038800121106     c                   eval      artdcm=bltdcm
038900121106     c                   eval      artcan=bltcan
039000121106    4c                   if        artcan<>' '
039100121106     c                   eval      artfta='P'
039200121106    4c                   endif
039300121106     c                   eval      artcca=bltfl1
039400121106     c                   eval      arttda=bltfl2
039500121106     c                   eval      artftr=bltftr
039600121106     c* Se bltftr=C significa trasmesso al cliente e quindi imposto
039700121106     c*  la data trasmissione al cliente
039800121106    4c                   if        bltftr='C'
039900121106     c                   eval      artdtc=artdcm
040000121106    4c                   endif
040100121106     c* Verifico se si tratta di bolla export
040200121106     c     bltlna        chain     azorg01l
040300121106    4c                   if        %found(azorg01l)
040400121106     c                   movel     orgde3        og143
040500121106     c                   else
040600121106     c                   clear                   og143
040700121106    4c                   endif
040800121106     c                   eval      artbga='C'
040900121106     c                   eval      artabn='S'
041000121106    6c                   if        §ogntw='EEX' or §ogntw='FED' or
041100121106     c                             §ogntw='DPD'
041200121106     c                   eval      artdmf=artdam
041300121106    6c                   endif
041400121106     c
041500121106     c     kar5          chain     fiar501l
041600121106    1c                   if        %found(fiar501l)
041700121106     c                   eval      dar5gen=ar5uni
041800121106    2c                   if        §AR5ARRDT>*zeros
041900121106     c                   movel     §AR5ARRDT     artdamf
042000121106     c                   movel     §AR5ARRHM     arthamf
042100121107     c                   eval      artnapf=artnap
042200121107     c
042300121106     c* se la linea finale di arrivo = al terminal --> aggiorno anche HAM
042400121106    3c                   if        artlna=blptfa  and artdam>0
042500121106     c                   movel     §AR5ARRHM     artham
042600121107     c                   movel     §AR5ARRDT     artdam
042700121106    3c                   endif
042800121106    2c                   endif
042900121106    1c                   endif
043000121106     C                   if        artdcm>0
043100121106     c
043200121106     c                   if        arthcm=0
043300121106     c                   movel     blphmc        arthcm
043400121106    1c                   endif
043500121106     c
043600121106     c* se negli eventi c'e' consegna parziale --> imposto per il dettaglio con data diversa
043700121106     c     kevb          setll     fnevb04l
043800121106     c     kevb          reade     fnevb04l
043900121106     c                   dow       not  %eof(fnevb04l)
044000121106     c*
044100121106     c* se la data consegna sul dett colli corrrisponde alla data evento
044200121106     c*  come orario imposto quello dell'evento
044300121106     c                   if        artdcm=evbdev
044400121106     c                   movel     evbhev        arthcm
044500121106     c                   leave
044600121106     c                   endif
044700121106     c
044800121106     c     kevb          reade     fnevb04l
044900121106     c                   enddo
045000121106     c                   endif
045100121106     c
045200121106     c
045300121106     c                   write     fiart000                             99
045400121106     c
045500000427     C     KBLP          READE     FNBLT00R
045600000427    2C                   ENDDO
045700000427      *
045800000427     C                   ENDIF
045900000427      *
046000000427     C                   ENDSR
046100000419      **-------------------------------------------------------------**
046200000419      **  AGGIORNAMENTO FILE LEGAMI                                  **
046300000419      **-------------------------------------------------------------**
046400041122     C**   AGGLEG        BEGSR
046500000419      *
046600041122     C**                 ADD       1             RECLBL
046700041122     C**   KLBL          SETLL     FNLBL02L
046800000419      *
046900041122    0C**                 IF        not %EQUAL(fnlbl02L)
047000041122     C**                 WRITE     FNLBL000
047100041122    1C**                 ENDIF
047200000419      *
047300041122     C**                 DELETE    FNLBLR
047400000419      *
047500041122     C**                 ENDSR
047600020205      **-------------------------------------------------------------**
047700020205      **  AGGIORNAMENTO FILES x C/ASSEGNI OMESSI                     **
047800020205      **-------------------------------------------------------------**
047900020205     C     srcass        BEGSR
048000060221      * Fiar400F solo record 8 e 9
048100060221     C     Kblp          SETLL     Fiar400R
048200020205     c                   do        *hival
048300060221     C     Kblp          READE     Fiar400R
048400060221    0C                   IF        %Eof(fiar400r)
048500020205     c                   leave
048600020205     c                   end
048700060221    0C                   IF        ar4trc <> '8' and ar4trc <> '9'
048800020205     c                   iter
048900020205     c                   end
049000060221     C     KBL89         DELETE    Fiar4000
049100060221     C                   z-add     udate8g       ar4dtr
049200060221     c                   movel     'R'           ar4ftr
049300020205      * monitorizzo l'errore in quanto nel file di ricezione potrei avere i
049400020205      * record doppi (2 richieste di ripristino consecutive)
049500060221     C                   WRITE     Fiar4000                             31
049600020205    2C                   ENDDO
049700051116      * FIAR900F
049800051116     C     KBLP          CHAIN     Fiar900R
049900051116    0C                   IF        %FOUND(fiar900r)
050000051116     C     KBLP          SETLL     Fiar901L
050100051116    0C                   if        not %EQUAL(fiar901l)
050200051116     C                   WRITE     Fiar9000
050300020205    1C                   endif
050400020205    1C                   ENDIF
050500021121      * FNBLP00F
050600060303     c*Aggiorno Oblpcbo, solo se il codice bolla non prevede C/A
050700060303     C                   MOVEL     '3A'          KCOD
050800060303     c                   movel(P)  oblpcbo       kkey
050900060303     C     KTAB1         CHAIN     TABEL00F                           30
051000060303     C  N30              MOVEL     TBLUNI        DS3a
051100060303     c   30              clear                   ds3a
051200060303     c                   if        §3afca=0
051300021121     c                   movel     blpcbo        oblpcbo
051400021121     c                   update    fnblp000
051500060303     c                   else
051600060303     c                   unlock    fnblp01l
051700060303     c                   endif
051800020205      *
051900020205     C                   ENDSR
052000000417      **-------------------------------------------------------------**
052100000417      **  CANCELLAZIONE BOLLE RICEVUTE                               **
052200000417      **-------------------------------------------------------------**
052300000417     C     DELRCD        BEGSR
052400000417      *
052500060221      * Fiar400F
052600060221     C     KBLP          DELETE    Fiar4R
052700060221     C                   DOW       %FOUND(fiar400r)
052800060221     C     KBLP          DELETE    Fiar4R
052900000417     C                   ENDDO
053000030110      *
053100030110      * FIAR500F
053200030110     C     KBLP          DELETE    FIAR5R
053300030110     C                   Dow       %FOUND(FIAR500R)
053400030110     C                   add       1             RecAR5
053500030110     C     KBLP          DELETE    FIAR5R
053600030110     C                   Enddo
053700000417      *
053800000417      * FIAR600F
053900000417     C     KBLP          DELETE    FIAR6R
054000000419     C                   DOW       %FOUND(fiar600r)
054100000417     C                   add       1             RECAR6
054200000417     C     KBLP          DELETE    FIAR6R
054300000417     C                   ENDDO
054400000417      *
054500000417      * FIAR700F
054600000417     C     KBLP          DELETE    FIAR7R
054700000419     C                   DOW       %FOUND(fiar700r)
054800000417     C                   add       1             RECAR7
054900000417     C     KBLP          DELETE    FIAR7R
055000000417     C                   ENDDO
055100000417      *
055200051116      * FiAR900F
055300051116     C     KBLP          DELETE    Fiar9R
055400051116     C                   DOW       %FOUND(fiar900r)
055500051116     C     KBLP          DELETE    Fiar9R
055600000417     C                   ENDDO
055700000417      *
055800000417      * FNBLT00F
055900000417     C     KBLP          DELETE    FNBLTR
056000000419     C                   DOW       %FOUND(fnblt00r)
056100000417     C                   add       1             RECBLT
056200000417     C     KBLP          DELETE    FNBLTR
056300000417     C                   ENDDO
056400000417      *
056500000417      * FNEVB00F
056600040831     C***  KBLP          DELETE    FNEVBR
056700040831     C***                DOW       %FOUND(fnevb00r)
056800040831     C***                add       1             RECEVB
056900040831     C***  KBLP          DELETE    FNEVBR
057000040831     C***                ENDDO
057100000417      *
057200000417      * FNBLP00F
057300000417     C                   DELETE    FNBLPR
057400000417      *
057500000417     C                   ENDSR
057600000413      **-------------------------------------------------------------**
057700000413      **  ESEGUE APERTURA SUI MEMBRI                                 **
057800000413      **-------------------------------------------------------------**
057900900526     C     CAEXA         BEGSR
058000000413      *
058100000413     c                   Do        Wnumele       XX
058200000413      *
058300000419      * Aggiungo il membro
058400000419     C                   MOVEL(P)  ScAdd(xx)     comman
058500000419     C                   CALL      'QCMDEXC'                            31
058600000413     C                   PARM                    comman
058700000413     C                   PARM                    lung
058800000413      *
058900000419      * Alloco il membro, se non riesco chiudo il PGM
059000000419     C                   MOVEL(P)  ScAlc(xx)     comman
059100000419     C                   CALL      'QCMDEXC'                            91
059200000419     C                   PARM                    comman
059300000419     C                   PARM                    lung
059400000419      *
059500000413     C   91              LEAVE
059600000413      *
059700000419      * Eseguo l'ovrdbf, se non riesco chiudo il pgm
059800000413     C                   MOVEL(P)  ScOvr(xx)     comman
059900000419     C                   CALL      'QCMDEXC'                            91
060000000413     C                   PARM                    comman
060100000413     C                   PARM                    lung
060200000413      *
060300000419     C   91              LEAVE
060400000419      *
060500000419     C                   ENDDO
060600000419      *
060700000413      * Apro i file
060800000419     C                   IF        *IN91 = *off
060900000413     C                   open      FNBLP00R
061000060221     C                   open      Fiar400R
061100030110     C                   open      FIAR500R
061200000413     C                   open      FIAR600R
061300000413     C                   open      FIAR700R
061400051116     C                   open      Fiar900R
061500000413     C                   open      FNBLT00R
061600041122     C***                open      FNLBL00R
061700040831     C****               open      FNEVB00R
061800000419     C                   ENDIF
061900000413      *
062000000413     C                   ENDSR
062100000419      *---------------------------------------------------------------*
062200000419      * Chiusura membro trascodificato e aggiornamento archivi di log
062300000419      *---------------------------------------------------------------*
062400000419     C     AGGcmric      BEGSR
062500000419      *
062600000419     C                   close     FNBLP00R
062700060221     C                   close     Fiar400R
062800030110     C                   close     FIAR500R
062900000419     C                   close     FIAR600R
063000000419     C                   close     FIAR700R
063100051116     C                   close     Fiar900R
063200000419     C                   close     FNBLT00R
063300041122     C***                close     FNLBL00R
063400040831     C****               close     FNEVB00R
063500000419      *
063600000419     C                   ENDSR
063700000413      *---------------------------------------------------------------*
063800000413      * SUBROUTINE INIZIALE
063900000413      *---------------------------------------------------------------*
064000000413     C     *INZSR        BEGSR
064100000413      *
064200000413     C     *ENTRY        PLIST
064300000413     C                   PARM                    KPJBA
064400000413      *
064500000413     c                   EVAL      Wnumele = %elem(ScAlc)
064600000413      *
064700000413     C                   Z-ADD     1             CODUT
064800000413     C                   CALL      'X§PARUT'
064900000413     C                   PARM                    UT§DSE0F
065000000413     C                   MOVEL     RAGUT         RSUT
065100000413     C                   MOVEL     REC80         CNCR80
065200000413      *
065300000413      * LA DATA DEL GIORNO LA PRENDO DA TIME
065400000413     C                   TIME                    W0140
065500000413     C                   MOVE      W0140         UDATE8
065600000414      * UDATE IN AAAAMMGG
065700000414     C     *eur          MOVEL     UDATE8        Wdata
065800000414     C     *iso          MOVEL     Wdata         UDATE8G
065900000413      *
066000041122     C**   KLBL          KLIST
066100041122     C**                 KFLD                    LBLAAP
066200041122     C**                 KFLD                    LBLLPP
066300041122     C**                 KFLD                    LBLNRP
066400041122     C**                 KFLD                    LBLNSP
066500041122     C**                 KFLD                    LBLAAN
066600041122     C**                 KFLD                    LBLLPN
066700041122     C**                 KFLD                    LBLNRN
066800041122     C**                 KFLD                    LBLNSN
066900000419      *
067000000419     C     KBLP          KLIST
067100000419     C                   KFLD                    BLPAAS
067200000419     C                   KFLD                    BLPLNP
067300000419     C                   KFLD                    BLPNRS
067400000419     C                   KFLD                    BLPNSP
067500121106     c     Kar5          Klist
067600121106     c                   Kfld                    artaas
067700121106     c                   Kfld                    artlnp
067800121106     c                   Kfld                    artnrs
067900121106     c                   Kfld                    artnsp
068000121106     c                   Kfld                    ktrd
068100121106     c     KEVB          Klist
068200121106     c                   Kfld                    artaas
068300121106     c                   Kfld                    artlnp
068400121106     c                   Kfld                    artnrs
068500121106     c                   Kfld                    artnsp
068600121106     c                   Kfld                    kcev
068700020205      *
068800020205     C     KBL89         KLIST
068900020205     C                   KFLD                    blPAAS
069000020205     C                   KFLD                    blPLNP
069100020205     C                   KFLD                    blPNRS
069200020205     C                   KFLD                    blPNSP
069300060221     C                   KFLD                    ar4TRC
069400000426      *
069500000426     C     KAR6          KLIST
069600000426     C                   KFLD                    ar6AAS
069700000426     C                   KFLD                    ar6LNP
069800000426     C                   KFLD                    ar6NRS
069900000426     C                   KFLD                    ar6NSP
070000000426     C                   KFLD                    ar6TRC
070100000427      *
070200000427     C     KBLT          KLIST
070300000427     C                   KFLD                    bltFLS
070400000427     C                   KFLD                    bltLNA
070500000427     C                   KFLD                    bltNRS
070600000427     C                   KFLD                    bltNSC
070700060303     C     KTAB1         KLIST
070800060303     C                   KFLD                    CODUT             1 0
070900060303     C                   KFLD                    KCOD              2
071000060303     C                   KFLD                    KKEY              8
071100000413      *
071200000413     C                   ENDSR
071300000413      *---------------------------------------------------------------*
071400000419** SCADD
071500000419ADDPFM FILE(FNBLP00R  )                 MBR(SEDE)
071600060221ADDPFM FILE(FIAR400R  )                 MBR(SEDE)
071700030110ADDPFM FILE(FIAR500R  )                 MBR(SEDE)
071800000419ADDPFM FILE(FIAR600R  )                 MBR(SEDE)
071900000419ADDPFM FILE(FIAR700R  )                 MBR(SEDE)
072000051116ADDPFM FILE(FIAR900R  )                 MBR(SEDE)
072100000419ADDPFM FILE(FNBLT00R  )                 MBR(SEDE)
072200000413** SCALC
072300000413ALCOBJ OBJ((FNBLP00R *FILE *EXCL           SEDE))
072400060221ALCOBJ OBJ((FIAR400R *FILE *EXCL           SEDE))
072500030110ALCOBJ OBJ((FIAR500R *FILE *EXCL           SEDE))
072600000413ALCOBJ OBJ((FIAR600R *FILE *EXCL           SEDE))
072700000413ALCOBJ OBJ((FIAR700R *FILE *EXCL           SEDE))
072800051116ALCOBJ OBJ((FIAR900R *FILE *EXCL           SEDE))
072900000413ALCOBJ OBJ((FNBLT00R *FILE *EXCL           SEDE))
073000000413** SCOVR
073100000419OVRDBF FILE(FNBLP00R  )                 MBR(SEDE)
073200060221OVRDBF FILE(FIAR400R  )                 MBR(SEDE)
073300030110OVRDBF FILE(FIAR500R  )                 MBR(SEDE)
073400000419OVRDBF FILE(FIAR600R  )                 MBR(SEDE)
073500000419OVRDBF FILE(FIAR700R  )                 MBR(SEDE)
073600051116OVRDBF FILE(FIAR900R  )                 MBR(SEDE)
073700000419OVRDBF FILE(FNBLT00R  )                 MBR(SEDE)
