000100030214     H DECEDIT('0,') DATEDIT(*YMD.)
000200120827     h dftactgrp(*no) actgrp(*caller)
000300120827ab   H BNDDIR('TIBS') EXTBININT(*YES) option(*nodebugio)
000400100317      *--------------------------------------------------------------*
000500030206      *      Conferma valorizzazione per i servizi T e P
000600911204      *--------------------------------------------------------------*
000700030206     FFicn64D   CF   E             WORKSTN
000800030206     FTABEL00F  IF   E           K DISK
000900030206     FfiFpT01L  IF   E           K DISK
001000030206     Ffiapd01L  if   E           K DISK
001100030206     Ffictt03l  uf a E           k DISK
001200940809     D*
001300950314RM*  D L6              S              3  0 DIM(30) ASCEND
001400121031     D savpjbu         S                   like(KPJBU)
001500121031     D ficng5DS      E DS
001600030206     D fnlv24ds      E DS
001700131007     D tibs42ds      E DS                  inz
001800131007     D uteautds      E DS                  inz
001900021129     D KPJBA         E DS
002000030206     D CNCR80        E DS
002100030206     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200030214     D                SDS
002300030214     D nompgm            *PROC
002400940909     D* DS PER TRUL06R - CARICAMENTO £X
002500940909     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002600950314RM*  D  LIN                    1     90  0
002700950314     D                                     DIM(30)
002800950314RM*  D                 DS
002900030206RM*  D  V1CPDR                 1      7
003000950314RM*  D  V1FPAD                 1      3  0
003100030206RM*  D  V1CPAD                 4      7
003200030206     D param           DS
003300030206     D  data                   1      8s 0
003400030210     D  anno                   1      4s 0
003500030210     D  mese                   5      6s 0
003600030206     D  codice                        7s 0
003700030206     D  tipopre                       1
003800030206     d  dsfgtout                           like(fgtds)
003900030206     d  error                         1
004000030206     d fgtds         e ds                  extname(fifgt00f)
004100030207     d ds1z          e ds
004200030214     d data1           s                   like(data)
004300030214     d com03           s              3
004400030210     d dataiso         s               d   inz
004500120827ab   ***************************************************************************
004600120827ab   ** Prototipi.
004700120827ab   ***************************************************************************
004800120827ab    **
004900120827ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
005000120827ab    /DEFINE DFTACTGRP_NO
005100120827ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
005200120827ab    /UNDEFINE DFTACTGRP_NO
005300120827ab   **
005400120827ab   ***************************************************************************
005500120827ab   **       Campi
005600120827ab   ***************************************************************************
005700120827ab   D TibsSof_esito...
005800120827ab   D                 S             10I 0 IMPORT
005900120827ab   D idSocieta...
006000120827EDPDCD                 S              3A
006100120827ab   **
006200120827EDPDCd Filiale         s              3S 0
006300120827EDPDCd dtVALIDITA      s               D
006400120827EDPDCd VALdatFINE      s               D
006500120827EDPDCd VALdatINIZ      s               D
006600120827EDPDCd SOC_errata      s              1A
006700920921     C*****************************************************************
007800100317     c
007900030206RM*  C                   exsr      azzera
008000030206     c*
008100030206     c                   do        *hival
008200030206     c                   if        *in99
008300030206     c                   setoff                                       99
008400030206     c                   write     y64d01
008500030206     c                   seton                                        99
008600030206     c                   end
008700030206     C                   exfmt     y64d01
008800920918     C* CMD3 - FINE LAVORO
008900030206     C   KC              leave
009000030206     c* controlli
009100030206     c                   exsr      srctr
009200030206     c   99              iter
009300121031     C*
009400121031     C*  esegue controlli sulle FIRME
009500121031     C                   EXSR      Ctrl_FIRME
009600121031     C*  se deve essere bloccato può solo fare F3 e uscire da questo pgm
009700121031     c                   if          devo_bloccare = 'S'
009800121031     C                   iter
009900121031     c                   end
010000121031      *
010100121031     c
010200030206     c* scrivi
010300030206     c                   if        *inkf and impov > 0
010400030206     c                   exsr      scrivi
010500030417RM*  C                   exsr      azzera
010600030206     c                   end
010700030206     c                   enddo
010800030206      *
010900100317     c     fine          tag
011000030206     C                   SETON                                        LR
011100030206     C*****************************************************************
011200030206     c     srctr         begsr
011300030206     C*****************************************************************
011400030206     c                   setoff                                       99
011500030206     c                   do
011600030206RM*  C* Filiale diversa da 0:
011700030129     c                   select
011800030212     c                   when      simtpp <> 'S'
011900950314RM*  C* deve essere in £6 se secondo livello
012000950314RM*  C     V1FPAD        LOOKUP    L6                                     45
012100950314RM*  C* Filiale non presente in tabella
012200950314RM* 3C     *IN45         IFEQ      *OFF
012300030206RM*  C                   SETON                                        9901
012400030206     c                   leave
012500950314RM* 3C                   END
012600030212RM* 3C                   ENDsl
012601130812      *controllo se utente autorizzato alla funzione
012602131007     c                   clear                   tibs42ds
012603131007     c                   movel     v1fpad        i42pge
012605131007     c                   call      'TIBS42R'
012606131007     c                   parm                    tibs42ds
012607131007     c                   movel     o42uni        uteautds
012608130812     c                   if        §autconc <> 'S'
012609130812     C                   SETON                                        9908
012610130812     c                   leave
012611130812     c                   endif
012700030206     C* CODICE COOP  SEMPRE > 0
012800950314RM*  C     V1CPAD        IFEQ      *BLANKS
012900950314RM*  C     V1CPAD        OREQ      *ZERO
013000030206     C                   SETON                                        9902
013100030206     c                   leave
013200920918     C                   END
013300030214     C* ricerca cooperativa
013400030214     C     '?'           SCAN      v1cpad                                 90
013500030214     C     *IN90         IFEQ      *ON
013600030214     C                   MOVEL     *BLANKS       d24rsc
013700030214     C                   Z-ADD     0             d24pdr
013800030214     C                   Z-ADD     V1fpad        d24FIL
013900030214     C                   MOVEL     'R'           d24FLG
014000030214     C                   MOVEL     'C'           d24tip
014100030214     C                   MOVEL(p)  fnlv24ds      KPJBU
014200030214     C                   CALL      'FNLV24R'
014300030214     C                   PARM                    KPJBA
014400030214     C                   MOVEL     KPJBU         fnlv24ds
014500030214     C                   MOVEl     d24pdr        V1fpad
014600030214     C                   MOVE      d24pdr        V1cpad
014700030214     C                   MOVEL     d24rsc        descop
014800030214     c                   else
014900021129     C* CONTROLLO VALIDITA' CODICE AUTO/COOP
015000940809     C                   TESTN                   V1CPDR               50
015100950314RM*  C                   MOVE      V1CPDR        W001A             1
015200950314RM*  C     *IN50         IFEQ      *OFF
015300950314RM*  C     W001A         ORLT      '0'
015400030206     C                   SETON                                        9902
015500030206     c                   leave
015600950314RM*  C                   END
015700030214     C                   ENDIF
015800920921     C*
015900030206     c                   clear                   descop
016000030206     C                   MOVEL     V1CPDR        KPDR
016100030206     C     Kapd          CHAIN     fiapd01L
016200030206    1C                   IF        not %found(fiapd01l)
016300030206    1C                             or (%found(fiapd01l) and apdatb <> ' ')
016400030206     C                   SETON                                        9902
016500030206     c                   leave
016600000726     C                   ENDIF
016700100223     C                   MOVEL     APDRSf        descop
016800030206     c* mese
016900030206     c                   if        mesev < 1 or mesev > 12
017000030206     C                   SETON                                        9905
017100030206     c                   leave
017200030206     c                   end
017300030206     c* anno
017400030206     c                   if        annov = 0
017500030206     C                   SETON                                        9906
017600030206     c                   leave
017700030206     c                   end
017800030206     c* tipo servizio
017900030206     c                   clear                   destsr
018000030206     c                   if        tsrv <> 'T' and tsrv <> 'P'
018100030206     C                   SETON                                        9907
018200030206     c                   leave
018300030206     c                   else
018400030206     c                   eval      tblkey = tsrv
018500030206     c     ktab          chain     tabel00f
018600030206     c                   if        %found(tabel00f)
018700030207     c                   movel     tbluni        ds1z
018800030207     c                   movel     §1zdes        destsr
018900030206     c                   end
019000030206     c                   end
019100030206     c* controllo che non esista già la valorizzazione del mese scelto
019200030206     c                   clear                   param
019300030210     c                   move      01            data
019400030210     c                   if        mesev = 12
019500030210     c                   move      01            mese
019600030210     c                   else
019700030210     c                   move      mesev         mese
019800030210     c                   add       1             mese
019900030210     c                   end
020000030210     c                   move      annov         anno
020100040105     c                   if        mesev = 12
020200040105     c                   add       1             anno
020300040105     c                   end
020400030210     c                   move      data          dataiso
020500030210     c                   subdur    1:*d          dataiso
020600030210     c                   move      dataiso       data
020700030206     c*
020800030206     c     kctt          chain     fictt03l
020900030213     c                   if        %found(fictt03l) and cttfvl = 'C'
021000030206     c                   seton                                        0499
021100030206     c                   leave
021200030206     c                   end
021300120828     C* CONTROLLO LA SOCIETÀ OPERATIVA
021400120828     C                   exsr      srsoc
021500120828     C                   if        soc_errata ='S'
021600120829     c                   seton                                        99
021700120828     c                   leave
021800120828     C                   end
021900030206     c* funzione per reperire la tariffa
022000030521     c                   clear                   impov
022100030206     c                   movel     v1fpad        codice
022200030206     c                   move      v1cpdr        codice
022300030206     c                   eval      tipopre = tsrv
022400030206     c                   call      'FICN36R3'
022500030206     c                   parm                    param
022600030206     c                   if        error <> *blanks
022700030206     c                   seton                                        0399
022800030206     c                   leave
022900030206     c                   else
023000030206     c                   eval      fgtds = dsfgtout
023100030206     c                   end
023200030206     c* calcolo l'importo del mese
023300030206     c     kfpt          setll     fifpt01l
023400030206     c                   do        *hival
023500030206     c     kfpt          reade     fifpt01l
023600030206     c                   if        %eof(fifpt01l)
023700030206     c                   leave
023800030206     c                   end
023900030206     c                   add       fptata        impov
024000030206     c                   enddo
024100030206     c                   enddo
024200030206     c*
024300030206     C                   ENDSR
024400940809     C**************************************************************************
024500120828     C     srsoc         BEGSR
024600030206     C**************************************************************************
024700120828     c                   eval      soc_errata = 'N'
024800030214     c* mi metto in loop xchè posso avere dei record non confermati inseriti
024900030214     c* per delle rettifiche
025000030214     c                   move      data          data1
025100030214     c                   move      01            data1
025200030214     c* da inizio mese fino alla fine
025300030214     c     kctt1         setll     fictt03l
025400030214     c                   do        *hival
025500120828     c     kctt2         reade(n)  fictt03l
025600030214     c                   if        %eof(fictt03l)
025700030214     c                   leave
025800030214     c                   end
025900030214     c* controllo data limite mese
026000030214     c                   if        cttddc > data
026100030214     c                   leave
026200030214     c                   end
026300120828     c* altrimenti controllo la società operativa
026400030214     c                   if        cttfvl <> 'C'
026500120829     c* controllo che la società operativa della filiale sia corretta
026600120829     c* nella memorizzazione delle rettifiche fatte durante il mese
026700120829     c                   if        cttddc < data
026800120828     c                   move      cttddc        dataiso
026900120828     C                   move      dataiso       dtVALIDITA
027000120828     C                   MOVEL     apdPDR        Filiale
027100120828     c                   exsr      chk_SOC_ope
027200120829     C                   if        soc_errata ='S' or
027300120829     c                             apdcsf <> idsocieta
027400120828     c                   eval      soc_errata = 'S'
027500120829     c                   seton                                        09
027600120828     c                   leave
027700120828     c                   end
027800120829     c                   end
027900120828     c                   end
028000120828     c                   enddo
028100120828     c* controllo l'ultimo gg del mese nel caso non sia stato già inserito
028200120828     c                   if        soc_errata = 'N'
028300120828     c                   move      data          dataiso
028400120828     C                   move      dataiso       dtVALIDITA
028500120828     C                   MOVEL     apdPDR        Filiale
028600120828     c                   exsr      chk_SOC_ope
028700120828     C                   if        soc_errata ='S' or
028800120828     c                             apdcsf <> idsocieta
028900120828     c                   eval      soc_errata = 'S'
029000120829     c                   seton                                        09
029100120828     c                   leavesr
029200120828     c                   end
029300120828     c                   end
029400120828     c                   endsr
029500120828     C**************************************************************************
029600120828     C     SCRIVI        BEGSR
029700120828     C**************************************************************************
029800120828     c                   clear                   fictt000
029900120828     c* mi metto in loop xchè posso avere dei record non confermati inseriti
030000120828     c* per delle rettifiche
030100120828     c                   move      data          data1
030200120828     c                   move      01            data1
030300120828     c* da inizio mese fino alla fine
030400120828     c     kctt1         setll     fictt03l
030500120828     c                   do        *hival
030600120828     c     kctt2         reade     fictt03l
030700120828     c                   if        %eof(fictt03l)
030800120828     c                   leave
030900120828     c                   end
031000120828     c* controllo data limite mese
031100120828     c                   if        cttddc > data
031200120828     c                   leave
031300120828     c                   end
031400120828     c* se fine mese aggiorno con l'importo della prestazione
031500120828     c                   if        cttddc = data
031600120828     c                   eval      cttitc = impov
031700120828     c                   end
031800120828     c* altrimenti confermo solo la riga
031900120828     c                   if        cttfvl <> 'C'
032000030214     c                   eval      cttfvl = 'C'
032100030214     c                   MOVE      *DATE         CTTDCV
032200030214     c                   end
032300120827     c                   move      *all'0'       cttcdf
032400120827     c                   move      apdksc        cttcdf
032500120827     c                   move      apdcsf        cttsoc
032600030214     c                   update    fictt000
032700030214     c                   enddo
032800030214     c* controllo se è già stata inserita la valorizzazione di fine mese
032900030415     c                   clear                   fictt000
033000030214     c     kctt          chain     fictt03l
033100030213     c                   if        not %found(fictt03l)
033200030214     c                   eval      cttfvl = 'C'
033300030214     c                   MOVE      *DATE         CTTDCV
033400030214     c                   eval      cttitc = impov
033500030206     c                   eval      cttddc = data
033600030206     c                   eval      cttpdr = codice
033700030206     c                   eval      cttfgs = v1fpad
033800030206     c                   eval      cttdiv = 'EUR'
033900030206     c                   eval      ctttsr = tsrv
034000030206     c                   eval      cttsoc = apdcsf
034100030206     c                   movel     *all'0'       cttcdf
034200030206     c                   move      apdksc        cttcdf
034300030206     c                   write     fictt000
034400030213     c                   end
034500920921     C                   ENDSR
034600030206     C*****************************************************************
034700030206     c     azzera        begsr
034800030206     C*****************************************************************
034900030206     c                   move      simpou        v1fpad
035000030206     c                   clear                   v1cpad
035100030206     c                   clear                   descop
035200030206     c                   clear                   mesev
035300030206     c                   clear                   annov
035400030206     c                   clear                   tsrv
035500030206     c                   clear                   destsr
035600030206     c                   clear                   impov
035700030206     c                   endsr
035800120827ab   C**************************************************************************
035900120827ab    * Controlla la Società operativa della Filiale
036000120827ab   C**************************************************************************
036100120827ab   C     CHK_SOC_OPE   begsr
036200120827ab   **
036300120827     c                   eval      soc_errata ='N'
036400120827ab    /FREE
036500120827ab
036600120827EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( Filiale
036700120827EDPDC                                             : dtVALIDITA
036800120827EDPDC                                             : 'O'
036900120827EDPDC                                             : valDatIniz
037000120827EDPDC                                             : valDatFine
037100120827ab                                                );
037200120827ab
037300120827EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
037400120827EDPDC    // Gestire l'errore.
037500120827
037600120827         eval  soc_errata ='S';
037700120827
037800120827ab     ENDIF;
037900120827ab
038000120827ab    /END-FREE
038100120827ab   C                   ENDSR
038200940809     C**************************************************************************
038300940809     C     *INZSR        BEGSR
038400030206     C**************************************************************************
038500030206     C*
038600030206     C     *ENTRY        PLIST
038700030206     C                   PARM                    KPJBA
038800030206     C                   Z-ADD     1             CODUT
038900030206     C                   CALL      'X§PARUT'
039000030206     C                   PARM                    UT§DSE
039100030206     C                   MOVEL     RAGUT         rsut
039200030206     C                   MOVEL     REC80         CNCR80
039300940909     C*
039400030206     C     *LIKE         DEFINE    APDPDR        KPDR
039500021129     C     Kapd          KLIST
039600030206     C                   KFLD                    apdtip
039700030206     C                   KFLD                    kpdr
039800030206     c                   eval      apdtip = 'C'
039900010207     C*
040000030206     C     KTAB          KLIST
040100030206     C                   KFLD                    tblkut
040200030206     C                   KFLD                    tblcod
040300030206     C                   KFLD                    tblkey
040400030206     C                   Z-ADD     1             tblkut
040500030206     C                   eval      tblcod = '1Z'
040600030206     C*
040700030206     C     Kctt          KLIST
040800030206     C                   KFLD                    v1fpad
040900030206     C                   KFLD                    kpdr
041000030206     C                   KFLD                    tsrv
041100030206     C                   KFLD                    data
041200030214     C*
041300030214     C     Kctt1         KLIST
041400030214     C                   KFLD                    v1fpad
041500030214     C                   KFLD                    kpdr
041600030214     C                   KFLD                    tsrv
041700030214     C                   KFLD                    data1
041800030214     C*
041900030214     C     Kctt2         KLIST
042000030214     C                   KFLD                    v1fpad
042100030214     C                   KFLD                    kpdr
042200030214     C                   KFLD                    tsrv
042300030206     C*
042400030206     C     Kfpt          KLIST
042500030206     C                   KFLD                    fgtpdr
042600030206     C                   KFLD                    fgtsml
042700030206     C                   KFLD                    fgttsr
042800030206     C                   KFLD                    fgtctr
042900030206     C                   KFLD                    fgtprg
043000030206RM*  C* CARICO LA TABELLA '£6' SE FILIALE DI SECONDO LIVELLO
043100030206RM*  C                   MOVE      '£6'          D06COD
043200030206RM*  C                   MOVEL     simpou        D06KEY
043300030206RM*  C                   MOVEL     DSUL06        KPJBU
043400030206RM*  C                   CALL      'TRUL06R'
043500030206RM*  C                   PARM                    KPJBA
043600030206RM*  C                   MOVEL     KPJBU         DSUL06
043700030206RM*  C                   MOVEA     LIN           L6
043800120829      *
043900120829ab   C*     Inizializza
044000120829ab    /FREE
044100120829ab     TibsSof_Init();
044200120829ab    /END-FREE
044300120829     C*
044400000808     C*
044500940809     C                   ENDSR
044600121031     C* --------------------------------------------------------------
044700121031     C*--- CONTROLLO sui DOCUMENTI Firmati della filiale -------------*
044800121031     C* --------------------------------------------------------------
044900121031     C     CTRL_FIRME    BEGSR
045000121031     C*
045100121031     C*  esegue un controllo sulle Valorizzazioni Firmate (NON solo x Autisti)
045200121031     C*   ma anche x COOP e AFFLUENZE/DEFLUENZE
045300121031     C*  Le firme devono essere a posto su tutta la filiale trattata altrimenti
045400121031     C*   viene emessa una Window di avvertimento e addirittura di BLOCCO.
045500121031     c                   move      'N'           devo_bloccare     1
045600121031     C                   MOVEL     kpjbu         savpjbu
045700121031     C*
045800121031      *  Inp-> §G5CNVL     1 A  C=CONTABILIZZAZIONE/V=VALORIZZAZIONI
045900121031      *  Inp-> §G5TIPO     1 A  BLANK=TUTTI/C=COOP/A=AUTCITTA/F=AFFDEF
046000121031      *  Inp-> §G5FIL      3 S0 FILIALE SOTTO CONTROLLO
046100121031      *  Out <-§G5FUN      2 A  TASTO FUNZIONALE PREMUTO
046200121031      *  Out <-§G5ESITO    1 A  F=FORZABILE/B=BLOCCANTE
046300121031     C*
046400121031     C                   clear                   kpjbu
046500121031     C                   clear                   ficng5ds
046600121031     c                   eval      §G5CNVL  = 'V'
046700121031     c                   eval      §G5FIL   = V1FPAD
046800121031     C                   MOVEL     ficng5ds      kpjbu
046900121031     c                   call      'FICNG5R'
047000121031     C                   parm                    kpjba
047100121031     C                   MOVEL     kpjbu         ficng5ds
047200121031     C                   clear                   kpjbu
047300121031     C                   MOVEL     savpjbu       kpjbu
047400121031     C*
047500121031     C*   se deve bloccare le valorizzazioni xchè la filiale è fuori tempo max
047600121031     C*    e devono firmare prima tutti i documenti in sospeso
047700121031     C*  è proprio il FICNG5R che ci dice questo e finchè non si sono messi a
047800121031     C*  posto con le firme non fa procedere con le valorizzazioni degli AUT
047900121031     c                   if        §G5ESITO ='B'
048000121031     c                   eval        devo_bloccare = 'S'
048100121031     c                   end
048200121031     C*
048300121031     C                   ENDSR
048400121031      *---------------------------------------------------------------------
