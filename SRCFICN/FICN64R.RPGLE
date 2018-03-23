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
002400950314RM*  D                 DS
002500030206RM*  D  V1CPDR                 1      7
002600950314RM*  D  V1FPAD                 1      3  0
002700030206RM*  D  V1CPAD                 4      7
002800030206     D param           DS
002900030206     D  data                   1      8s 0
003000030210     D  anno                   1      4s 0
003100030210     D  mese                   5      6s 0
003200030206     D  codice                        7s 0
003300030206     D  tipopre                       1
003400030206     d  dsfgtout                           like(fgtds)
003500030206     d  error                         1
003600030206     d fgtds         e ds                  extname(fifgt00f)
003700030207     d ds1z          e ds
003800030214     d data1           s                   like(data)
003900030214     d com03           s              3
004000030210     d dataiso         s               d   inz
004100120827ab   ***************************************************************************
004200120827ab   ** Prototipi.
004300120827ab   ***************************************************************************
004400120827ab    **
004500120827ab    /COPY GAITRASRC/SRCCONST,TIBSSOFR
004600120827ab    /DEFINE DFTACTGRP_NO
004700120827ab    /COPY GAITRASRC/SRCPROTOPR,TIBSSOFR
004800120827ab    /UNDEFINE DFTACTGRP_NO
004900120827ab   **
005000120827ab   ***************************************************************************
005100120827ab   **       Campi
005200120827ab   ***************************************************************************
005300120827ab   D TibsSof_esito...
005400120827ab   D                 S             10I 0 IMPORT
005500120827ab   D idSocieta...
005600120827EDPDCD                 S              3A
005700120827ab   **
005800120827EDPDCd Filiale         s              3S 0
005900120827EDPDCd dtVALIDITA      s               D
006000120827EDPDCd VALdatFINE      s               D
006100120827EDPDCd VALdatINIZ      s               D
006200120827EDPDCd SOC_errata      s              1A
006300920921     C*****************************************************************
006400100317     c
006500030206RM*  C                   exsr      azzera
006600030206     c*
006700030206     c                   do        *hival
006800030206     c                   if        *in99
006900030206     c                   setoff                                       99
007000030206     c                   write     y64d01
007100030206     c                   seton                                        99
007200030206     c                   end
007300030206     C                   exfmt     y64d01
007400920918     C* CMD3 - FINE LAVORO
007500030206     C   KC              leave
007600030206     c* controlli
007700030206     c                   exsr      srctr
007800030206     c   99              iter
007900121031     C*
008000121031     C*  esegue controlli sulle FIRME
008100121031     C                   EXSR      Ctrl_FIRME
008200121031     C*  se deve essere bloccato può solo fare F3 e uscire da questo pgm
008300121031     c                   if          devo_bloccare = 'S'
008400121031     C                   iter
008500121031     c                   end
008600121031      *
008700121031     c
008800030206     c* scrivi
008900030206     c                   if        *inkf and impov > 0
009000030206     c                   exsr      scrivi
009100030417RM*  C                   exsr      azzera
009200030206     c                   end
009300030206     c                   enddo
009400030206      *
009500100317     c     fine          tag
009600030206     C                   SETON                                        LR
009700030206     C*****************************************************************
009800030206     c     srctr         begsr
009900030206     C*****************************************************************
010000030206     c                   setoff                                       99
010100030206     c                   do
010200030206RM*  C* Filiale diversa da 0:
010300130812      *controllo se utente autorizzato alla funzione
010400131007     c                   clear                   tibs42ds
010500131007     c                   movel     v1fpad        i42pge
010600131007     c                   call      'TIBS42R'
010700131007     c                   parm                    tibs42ds
010800131007     c                   movel     o42uni        uteautds
010900130812     c                   if        §autconc <> 'S'
011000130812     C                   SETON                                        9908
011100130812     c                   leave
011200130812     c                   endif
011300030206     C* CODICE COOP  SEMPRE > 0
011400950314RM*  C     V1CPAD        IFEQ      *BLANKS
011500950314RM*  C     V1CPAD        OREQ      *ZERO
011600030206     C                   SETON                                        9902
011700030206     c                   leave
011800920918     C                   END
011900030214     C* ricerca cooperativa
012000030214     C     '?'           SCAN      v1cpad                                 90
012100030214     C     *IN90         IFEQ      *ON
012200030214     C                   MOVEL     *BLANKS       d24rsc
012300030214     C                   Z-ADD     0             d24pdr
012400030214     C                   Z-ADD     V1fpad        d24FIL
012500030214     C                   MOVEL     'R'           d24FLG
012600030214     C                   MOVEL     'C'           d24tip
012700030214     C                   MOVEL(p)  fnlv24ds      KPJBU
012800030214     C                   CALL      'FNLV24R'
012900030214     C                   PARM                    KPJBA
013000030214     C                   MOVEL     KPJBU         fnlv24ds
013100030214     C                   MOVEl     d24pdr        V1fpad
013200030214     C                   MOVE      d24pdr        V1cpad
013300030214     C                   MOVEL     d24rsc        descop
013400030214     c                   else
013500021129     C* CONTROLLO VALIDITA' CODICE AUTO/COOP
013600940809     C                   TESTN                   V1CPDR               50
013700950314RM*  C                   MOVE      V1CPDR        W001A             1
013800950314RM*  C     *IN50         IFEQ      *OFF
013900950314RM*  C     W001A         ORLT      '0'
014000030206     C                   SETON                                        9902
014100030206     c                   leave
014200950314RM*  C                   END
014300030214     C                   ENDIF
014400920921     C*
014500030206     c                   clear                   descop
014600030206     C                   MOVEL     V1CPDR        KPDR
014700030206     C     Kapd          CHAIN     fiapd01L
014800030206    1C                   IF        not %found(fiapd01l)
014900030206    1C                             or (%found(fiapd01l) and apdatb <> ' ')
015000030206     C                   SETON                                        9902
015100030206     c                   leave
015200000726     C                   ENDIF
015300100223     C                   MOVEL     APDRSf        descop
015400030206     c* mese
015500030206     c                   if        mesev < 1 or mesev > 12
015600030206     C                   SETON                                        9905
015700030206     c                   leave
015800030206     c                   end
015900030206     c* anno
016000030206     c                   if        annov = 0
016100030206     C                   SETON                                        9906
016200030206     c                   leave
016300030206     c                   end
016400030206     c* tipo servizio
016500030206     c                   clear                   destsr
016600030206     c                   if        tsrv <> 'T' and tsrv <> 'P'
016700030206     C                   SETON                                        9907
016800030206     c                   leave
016900030206     c                   else
017000030206     c                   eval      tblkey = tsrv
017100030206     c     ktab          chain     tabel00f
017200030206     c                   if        %found(tabel00f)
017300030207     c                   movel     tbluni        ds1z
017400030207     c                   movel     §1zdes        destsr
017500030206     c                   end
017600030206     c                   end
017700030206     c* controllo che non esista già la valorizzazione del mese scelto
017800030206     c                   clear                   param
017900030210     c                   move      01            data
018000030210     c                   if        mesev = 12
018100030210     c                   move      01            mese
018200030210     c                   else
018300030210     c                   move      mesev         mese
018400030210     c                   add       1             mese
018500030210     c                   end
018600030210     c                   move      annov         anno
018700040105     c                   if        mesev = 12
018800040105     c                   add       1             anno
018900040105     c                   end
019000030210     c                   move      data          dataiso
019100030210     c                   subdur    1:*d          dataiso
019200030210     c                   move      dataiso       data
019300030206     c*
019400030206     c     kctt          chain     fictt03l
019500030213     c                   if        %found(fictt03l) and cttfvl = 'C'
019600030206     c                   seton                                        0499
019700030206     c                   leave
019800030206     c                   end
019900120828     C* CONTROLLO LA SOCIETÀ OPERATIVA
020000120828     C                   exsr      srsoc
020100120828     C                   if        soc_errata ='S'
020200120829     c                   seton                                        99
020300120828     c                   leave
020400120828     C                   end
020500030206     c* funzione per reperire la tariffa
020600030521     c                   clear                   impov
020700030206     c                   movel     v1fpad        codice
020800030206     c                   move      v1cpdr        codice
020900030206     c                   eval      tipopre = tsrv
021000030206     c                   call      'FICN36R3'
021100030206     c                   parm                    param
021200030206     c                   if        error <> *blanks
021300030206     c                   seton                                        0399
021400030206     c                   leave
021500030206     c                   else
021600030206     c                   eval      fgtds = dsfgtout
021700030206     c                   end
021800030206     c* calcolo l'importo del mese
021900030206     c     kfpt          setll     fifpt01l
022000030206     c                   do        *hival
022100030206     c     kfpt          reade     fifpt01l
022200030206     c                   if        %eof(fifpt01l)
022300030206     c                   leave
022400030206     c                   end
022500030206     c                   add       fptata        impov
022600030206     c                   enddo
022700030206     c                   enddo
022800030206     c*
022900030206     C                   ENDSR
023000940809     C**************************************************************************
023100120828     C     srsoc         BEGSR
023200030206     C**************************************************************************
023300120828     c                   eval      soc_errata = 'N'
023400030214     c* mi metto in loop xchè posso avere dei record non confermati inseriti
023500030214     c* per delle rettifiche
023600030214     c                   move      data          data1
023700030214     c                   move      01            data1
023800030214     c* da inizio mese fino alla fine
023900030214     c     kctt1         setll     fictt03l
024000030214     c                   do        *hival
024100120828     c     kctt2         reade(n)  fictt03l
024200030214     c                   if        %eof(fictt03l)
024300030214     c                   leave
024400030214     c                   end
024500030214     c* controllo data limite mese
024600030214     c                   if        cttddc > data
024700030214     c                   leave
024800030214     c                   end
024900120828     c* altrimenti controllo la società operativa
025000030214     c                   if        cttfvl <> 'C'
025100120829     c* controllo che la società operativa della filiale sia corretta
025200120829     c* nella memorizzazione delle rettifiche fatte durante il mese
025300120829     c                   if        cttddc < data
025400120828     c                   move      cttddc        dataiso
025500120828     C                   move      dataiso       dtVALIDITA
025600120828     C                   MOVEL     apdPDR        Filiale
025700120828     c                   exsr      chk_SOC_ope
025800120829     C                   if        soc_errata ='S' or
025900120829     c                             apdcsf <> idsocieta
026000120828     c                   eval      soc_errata = 'S'
026100120829     c                   seton                                        09
026200120828     c                   leave
026300120828     c                   end
026400120829     c                   end
026500120828     c                   end
026600120828     c                   enddo
026700120828     c* controllo l'ultimo gg del mese nel caso non sia stato già inserito
026800120828     c                   if        soc_errata = 'N'
026900120828     c                   move      data          dataiso
027000120828     C                   move      dataiso       dtVALIDITA
027100120828     C                   MOVEL     apdPDR        Filiale
027200120828     c                   exsr      chk_SOC_ope
027300120828     C                   if        soc_errata ='S' or
027400120828     c                             apdcsf <> idsocieta
027500120828     c                   eval      soc_errata = 'S'
027600120829     c                   seton                                        09
027700120828     c                   leavesr
027800120828     c                   end
027900120828     c                   end
028000120828     c                   endsr
028100120828     C**************************************************************************
028200120828     C     SCRIVI        BEGSR
028300120828     C**************************************************************************
028400120828     c                   clear                   fictt000
028500120828     c* mi metto in loop xchè posso avere dei record non confermati inseriti
028600120828     c* per delle rettifiche
028700120828     c                   move      data          data1
028800120828     c                   move      01            data1
028900120828     c* da inizio mese fino alla fine
029000120828     c     kctt1         setll     fictt03l
029100120828     c                   do        *hival
029200120828     c     kctt2         reade     fictt03l
029300120828     c                   if        %eof(fictt03l)
029400120828     c                   leave
029500120828     c                   end
029600120828     c* controllo data limite mese
029700120828     c                   if        cttddc > data
029800120828     c                   leave
029900120828     c                   end
030000120828     c* se fine mese aggiorno con l'importo della prestazione
030100120828     c                   if        cttddc = data
030200120828     c                   eval      cttitc = impov
030300120828     c                   end
030400120828     c* altrimenti confermo solo la riga
030500120828     c                   if        cttfvl <> 'C'
030600030214     c                   eval      cttfvl = 'C'
030700030214     c                   MOVE      *DATE         CTTDCV
030800030214     c                   end
030900120827     c                   move      *all'0'       cttcdf
031000120827     c                   move      apdksc        cttcdf
031100120827     c                   move      apdcsf        cttsoc
031200030214     c                   update    fictt000
031300030214     c                   enddo
031400030214     c* controllo se è già stata inserita la valorizzazione di fine mese
031500030415     c                   clear                   fictt000
031600030214     c     kctt          chain     fictt03l
031700030213     c                   if        not %found(fictt03l)
031800030214     c                   eval      cttfvl = 'C'
031900030214     c                   MOVE      *DATE         CTTDCV
032000030214     c                   eval      cttitc = impov
032100030206     c                   eval      cttddc = data
032200030206     c                   eval      cttpdr = codice
032300030206     c                   eval      cttfgs = v1fpad
032400030206     c                   eval      cttdiv = 'EUR'
032500030206     c                   eval      ctttsr = tsrv
032600030206     c                   eval      cttsoc = apdcsf
032700030206     c                   movel     *all'0'       cttcdf
032800030206     c                   move      apdksc        cttcdf
032900030206     c                   write     fictt000
033000030213     c                   end
033100920921     C                   ENDSR
033200030206     C*****************************************************************
033300030206     c     azzera        begsr
033400030206     C*****************************************************************
033500030206     c                   move      simpou        v1fpad
033600030206     c                   clear                   v1cpad
033700030206     c                   clear                   descop
033800030206     c                   clear                   mesev
033900030206     c                   clear                   annov
034000030206     c                   clear                   tsrv
034100030206     c                   clear                   destsr
034200030206     c                   clear                   impov
034300030206     c                   endsr
034400120827ab   C**************************************************************************
034500120827ab    * Controlla la Società operativa della Filiale
034600120827ab   C**************************************************************************
034700120827ab   C     CHK_SOC_OPE   begsr
034800120827ab   **
034900120827     c                   eval      soc_errata ='N'
035000120827ab    /FREE
035100120827ab
035200120827EDPDC  idSocieta = TibsSof_GetIdSocietaByIdFiliale( Filiale
035300120827EDPDC                                             : dtVALIDITA
035400120827EDPDC                                             : 'O'
035500120827EDPDC                                             : valDatIniz
035600120827EDPDC                                             : valDatFine
035700120827ab                                                );
035800120827ab
035900120827EDPDC  IF idSocieta = *BLANK OR TibsSof_esito < TIBSSOF_ESITO_OK;
036000120827EDPDC    // Gestire l'errore.
036100120827
036200120827         eval  soc_errata ='S';
036300120827
036400120827ab     ENDIF;
036500120827ab
036600120827ab    /END-FREE
036700120827ab   C                   ENDSR
036800940809     C**************************************************************************
036900940809     C     *INZSR        BEGSR
037000030206     C**************************************************************************
037100030206     C*
037200030206     C     *ENTRY        PLIST
037300030206     C                   PARM                    KPJBA
037400030206     C                   Z-ADD     1             CODUT
037500030206     C                   CALL      'X§PARUT'
037600030206     C                   PARM                    UT§DSE
037700030206     C                   MOVEL     RAGUT         rsut
037800030206     C                   MOVEL     REC80         CNCR80
037900940909     C*
038000030206     C     *LIKE         DEFINE    APDPDR        KPDR
038100021129     C     Kapd          KLIST
038200030206     C                   KFLD                    apdtip
038300030206     C                   KFLD                    kpdr
038400030206     c                   eval      apdtip = 'C'
038500010207     C*
038600030206     C     KTAB          KLIST
038700030206     C                   KFLD                    tblkut
038800030206     C                   KFLD                    tblcod
038900030206     C                   KFLD                    tblkey
039000030206     C                   Z-ADD     1             tblkut
039100030206     C                   eval      tblcod = '1Z'
039200030206     C*
039300030206     C     Kctt          KLIST
039400030206     C                   KFLD                    v1fpad
039500030206     C                   KFLD                    kpdr
039600030206     C                   KFLD                    tsrv
039700030206     C                   KFLD                    data
039800030214     C*
039900030214     C     Kctt1         KLIST
040000030214     C                   KFLD                    v1fpad
040100030214     C                   KFLD                    kpdr
040200030214     C                   KFLD                    tsrv
040300030214     C                   KFLD                    data1
040400030214     C*
040500030214     C     Kctt2         KLIST
040600030214     C                   KFLD                    v1fpad
040700030214     C                   KFLD                    kpdr
040800030214     C                   KFLD                    tsrv
040900030206     C*
041000030206     C     Kfpt          KLIST
041100030206     C                   KFLD                    fgtpdr
041200030206     C                   KFLD                    fgtsml
041300030206     C                   KFLD                    fgttsr
041400030206     C                   KFLD                    fgtctr
041500030206     C                   KFLD                    fgtprg
041600120829      *
041700120829ab   C*     Inizializza
041800120829ab    /FREE
041900120829ab     TibsSof_Init();
042000120829ab    /END-FREE
042100120829     C*
042200000808     C*
042300940809     C                   ENDSR
042400121031     C* --------------------------------------------------------------
042500121031     C*--- CONTROLLO sui DOCUMENTI Firmati della filiale -------------*
042600121031     C* --------------------------------------------------------------
042700121031     C     CTRL_FIRME    BEGSR
042800121031     C*
042900121031     C*  esegue un controllo sulle Valorizzazioni Firmate (NON solo x Autisti)
043000121031     C*   ma anche x COOP e AFFLUENZE/DEFLUENZE
043100121031     C*  Le firme devono essere a posto su tutta la filiale trattata altrimenti
043200121031     C*   viene emessa una Window di avvertimento e addirittura di BLOCCO.
043300121031     c                   move      'N'           devo_bloccare     1
043400121031     C                   MOVEL     kpjbu         savpjbu
043500121031     C*
043600121031      *  Inp-> §G5CNVL     1 A  C=CONTABILIZZAZIONE/V=VALORIZZAZIONI
043700121031      *  Inp-> §G5TIPO     1 A  BLANK=TUTTI/C=COOP/A=AUTCITTA/F=AFFDEF
043800121031      *  Inp-> §G5FIL      3 S0 FILIALE SOTTO CONTROLLO
043900121031      *  Out <-§G5FUN      2 A  TASTO FUNZIONALE PREMUTO
044000121031      *  Out <-§G5ESITO    1 A  F=FORZABILE/B=BLOCCANTE
044100121031     C*
044200121031     C                   clear                   kpjbu
044300121031     C                   clear                   ficng5ds
044400121031     c                   eval      §G5CNVL  = 'V'
044500121031     c                   eval      §G5FIL   = V1FPAD
044600121031     C                   MOVEL     ficng5ds      kpjbu
044700121031     c                   call      'FICNG5R'
044800121031     C                   parm                    kpjba
044900121031     C                   MOVEL     kpjbu         ficng5ds
045000121031     C                   clear                   kpjbu
045100121031     C                   MOVEL     savpjbu       kpjbu
045200121031     C*
045300121031     C*   se deve bloccare le valorizzazioni xchè la filiale è fuori tempo max
045400121031     C*    e devono firmare prima tutti i documenti in sospeso
045500121031     C*  è proprio il FICNG5R che ci dice questo e finchè non si sono messi a
045600121031     C*  posto con le firme non fa procedere con le valorizzazioni degli AUT
045700121031     c                   if        §G5ESITO ='B'
045800121031     c                   eval        devo_bloccare = 'S'
045900121031     c                   end
046000121031     C*
046100121031     C                   ENDSR
046200121031      *---------------------------------------------------------------------
