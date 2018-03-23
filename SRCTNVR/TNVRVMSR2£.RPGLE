000100000111     H DECEDIT('0,') DATEDIT(*DMY.)
000200981201     F*-------------------------------------------------------------------------------------------*
000300160912     F* Il pgm cerca per ogni membro in carico le schiere a tempo di compilazione di un certo tipo
000400981201     F*-------------------------------------------------------------------------------------------*
000500981203     FINPUTBND  IF   F  112        DISK    USROPN
000600160914     FTIVMS01I  IF   E           K DISK    rename(TIVMS000:TIVMS01)
000700160909     FTIVMS00F  IF A E           K DISK    prefix(F_)
000800981201     D*-------------------------------------------------------------------------------------------*
000900140117     D* spezza la specifica
001000140117     DTotSpecDS        DS           112
001100140117     D SRCSEQ                  1      6  0                                      N° specifica
001200140117     D SRCDAT                  7     12  0                                      Data modifica spec.
001300160909     D SRCRigaInt             13    112                                         Riga intera
001400160909     D SRCSk                  13     15                                         '** '
001500160912     D SRCSkDes               16    112                                         Riga intera
001600160909     D SRCNUM                 13     17                                         N° contrassegno
001700140117     D SRCTIP                 18     18                                         Tipo specifica
001800140117     D SRCPAR                 19     23                                         '*' per commento
001900140117     D SRCFat1                24     37                                         Codice operativo
002000140117     D SRCCodOp               38     47
002100140117     D SRCFat2                48     61
002200140117     D SRCFat2Ext             48    112
002300140117     D SRCRisu                62     75
002400140117     D SRCSpeO                42     55
002500140626     Dpsds            sds
002600140626     D pgmname           *proc
002700140626     D pgmsts            *status
002800160909     D*-----------
002900160909     D* DS RIDEFINIZIONE SCHIERE A PROGRAMMA
003000160909     D*-----------
003100160909     D DS_WCAU_1       DS                  qualified
003200160909     D  dsCAU_BRT                    15
003300160909     D  dsCAU_CLI                    10
003400160909     D  dsCAU_OPZ                     5
003500160909     D  dsCAU_DES                    60
003600160909     D*
003700160909     D DS_WDCT_1       DS                  qualified
003800160909     D  dsDCT_BRT                    15
003900160909     D  dsDCT_CLI                    15
004000160909     D*
004100160909     D DS_WTIS9_1      DS                  qualified
004200160909     D  dsTIS9_BRT                   15
004300160909     D  dsTIS9_CLI                   15
004400160909     D*
004500160909     D DS_WCAU_2       DS                  qualified
004600160909     D  dsCAU_BRT                    15
004700160909     D  dsCAU_EVT                    10
004800160909     D  dsCAU_RSN                     5
004900160909     D  dsCAU_DES                    60
005000160909     D*
005100160909     D DS_WDCT_2       DS                  qualified
005200160909     D  dsDCT_BRT                    15
005300160909     D  dsDCT_EVT                    10
005400160909     D  dsDCT_RSN                     5
005500160909     D*
005600160909     D DS_WTIS9_2      DS                  qualified
005700160909     D  dsTIS9_BRT                   15
005800160909     D  dsTIS9_EVT                   10
005900160909     D  dsTIS9_RSN                    5
006000140117     D*---------                                  d                         ----------------------*
006100990604     D* flag di fine sorgente
006200990604     D $Esci           S              1A
006300140116     D*---------                                  d                         ----------------------*
006400140116     D* parametri
006500140117     d pIn_Lib         s             10a
006600140117     d pIn_File        s             10a
006700140625     d pIn_Mbr         s             10a
006800160909     d pIn_DS_Sel      ds
006900160909     d  pIn_TpSk                      1
007000160909     d  pIn_Sk1                      10
007100160909     d  pIn_Sk2                      10
007200160909     d  pIn_Sk3                      10
007300160912     d  pIn_Proc                     10
007400160912     d  pIn_KSU                       8
007500160912     d  pIn_DDE                       8s 0
007600160912     d  pIn_DSC                       8s 0
007700140116     d pIn_MLNAME      s             10a
007800140116     d pIn_MLMTXT      s             50a
007900140117     d pOut_Esito      s              2  0
008000140625     D*---------                                                            ----------------------*
008100160909     d wPos            s              4  0
008200160912     d wIdSk           s             10
008300160909     d wAttivo         s               n   inz(*off)
008400160912     d wWrite          s               n   inz(*off)
008500140116     D*------------------
008600140116     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008700140116     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008800140114     D*---------                                                            ----------------------*
008900000110     I* Tracciato record file sorgente in lettura
009000981126     IINPUTBND  NS  01
009100140117     I                                  1  112  TotSpec                         Specifica
009200000110     I*---------                                                            ----------------------*
009300981201     C*-------------------------------------------------------------------------------------------*
009400981201     C*  FLUSSO PRINCIPALE                                                                        *
009500981201     C*-------------------------------------------------------------------------------------------*
009600981201     C*
009700981201     C     *ENTRY        PLIST
009800140116     C                   PARM                    pIn_Lib
009900140116     C                   PARM                    pIn_File
010000140625     C                   PARM                    pIn_Mbr
010100160909     C                   PARM                    pIn_DS_Sel
010200140116     C                   PARM                    pIn_MLNAME
010300140116     C                   PARM                    pIn_MLMTXT
010400140116     C                   PARM                    pOut_Esito
010500981201     C*
010600140116     C* controlli sul membro che non fa il CLLE
010700140116     C                   EXSR      CtrlMbr
010800140114     C*
010900981126     C                   OPEN      INPUTBND
011000990604     C*
011100160912     C                   DoU       $Esci = *On
011200160912     C*
011300160912     C                   READ      INPUTBND                               21
011400160912     C                   MOVE      *In21         $Esci
011500160912     C*
011600160912     C                   if        $Esci <> *On
011700140116     C*
011800140116     C* maiuscolizzo la specifica
011900140117     C                   EVAL      TotSpecDS = %xlate(minu:maiu:TotSpec)
012000000111     C*
012100160909     C* 2) il pgm si deve "attivare" quando comincia a trovare delle schiere a tempo di compilazione
012200160909     C*    che contengono un nome nella descrizione, e questo coincide con uno dei 3 passati
012300160909     C*
012400160909     C                   If        SrcSk = '** '
012500160909     C* parto da disattivo (potrebbe essere la 2a schiera a tempo di compilazione che trovo)
012600160909     C                   eval      wAttivo = *off
012700160912     C* prima provo a cercare un - per dividere il nome schiera dalla pura descrizione
012800160912     c                   eval      wPos = %scan('-':%subst(SrcSkDes:1:97))
012900160909     c                   if        wPos = 0
013000160909     C* se non lo trovo, provo a cercare un blank
013100160912     c                   eval      wPos = %scan(' ':%subst(SrcSkDes:1:97))
013200160909     c                   if        wPos = 0
013300160909     C* se non lo trovo (impossibile), salto alla prossima riga
013400160909     c                   iter
013500160909     c                   endif
013600160909     c                   endif
013700160909     c*
013800160909     c* scarico la porzione di schiera trovata in una variabile per controllarne la dimensione
013900160909     c                   eval      wIdSk =
014000160912     c                              %subst(SrcSkDes:1:wPos-1)
014100160909     c* se la dimensione è > 10 non può essere il nome schiera, per cui salto alla prossima riga
014200160909     c                   if        %len(%trim(wIdSk)) > 10
014300160909     c                   iter
014400160909     c                   endif
014500160909     c*
014600160909     c* testo il nome
014700160909     c                   if        %trim(wIdSk) <> %trim(pIn_Sk1) and
014800160909     c                             %trim(wIdSk) <> %trim(pIn_Sk2) and
014900160909     c                             %trim(wIdSk) <> %trim(pIn_Sk3)
015000160909     c* se non è uno dei 3 nomi richiesti, salto alla prossima riga
015100160909     c                   iter
015200160909     c                   endif
015300160909     c*
015400160909     c* se sono qui vuol dire che è una delle schiere da prendere in considerazione
015500160909     C                   eval      wAttivo = *on
015600160909     c                   iter
015700160909     c                   else
015800160909     C                   if        wAttivo = *off
015900160909     c                   iter
016000160909     c                   endif
016100160909     c                   endif
016200160909     c*
016300160909     c* se sono qui vuol dire che sono in una riga di schiera a tempo di compilazione
016400160909     c* (non nella riga del nome ma proprio nell'elemento di schiera)
016500160909     c*
016600160912     c* 3) lo spezzo a seconda del nome e del tipo schiera
016700160909     c                   select
016800160909     c                   when      %trim(wIdSk) = %trim(pIn_Sk1)
016900160909     c                   select
017000160909     c                   when      pIn_TpSk = '1'
017100160912     c                   eval      DS_WCAU_1 = SRCRigaInt
017200160909     c                   when      pIn_TpSk = '2'
017300160912     c                   eval      DS_WCAU_2 = SRCRigaInt
017400160909     c                   endsl
017500160909     c                   when      %trim(wIdSk) = %trim(pIn_Sk2)
017600160909     c                   select
017700160909     c                   when      pIn_TpSk = '1'
017800160912     c                   eval      DS_WDCT_1 = SRCRigaInt
017900160909     c                   when      pIn_TpSk = '2'
018000160912     c                   eval      DS_WDCT_2 = SRCRigaInt
018100160909     c                   endsl
018200160909     c                   when      %trim(wIdSk) = %trim(pIn_Sk3)
018300160909     c                   select
018400160909     c                   when      pIn_TpSk = '1'
018500160912     c                   eval      DS_WTIS9_1 = SRCRigaInt
018600160909     c                   when      pIn_TpSk = '2'
018700160912     c                   eval      DS_WTIS9_2 = SRCRigaInt
018800160909     c                   endsl
018900160909     c                   endsl
019000160912     c*
019100160912     c* 4) scrivo il rcd trovato
019200160912     c*    se la specifica di sorgente non andava bene, ho iterato
019300160912     c                   exsr      WriteVMS
019400160912     c*
019500160912     C                   endif
019600160912     c*
019700160912     C                   EndDo
019800990604     C*
019900160912     C* Se alla fine del sorgente ho scritto, passo esito 0, altrimenti 1
020000160912     c                   eval      pOut_Esito = 0
020100160912     C                   If        wWrite = *off
020200160912     c                   eval      pOut_Esito = 2
020300000110     C                   EndIf
020400000110     C*
020500981126     C                   CLOSE     INPUTBND
020600981126     C*
020700980325     C                   SETON                                            RT
020800981126     C*
020900160912     C*------------------------------------------------------------------------*
021000160912     C* *INZSR
021100160912     C*------------------------------------------------------------------------*
021200160912     C     *INZSR        BEGSR
021300160912     C*
021400160912     C* LETTURA TIVMS05I - COMPLETA
021500160914     C     K05VMS01      KLIST
021600160912     C                   KFLD                    VMSProc
021700160912     C                   KFLD                    VMSKSU
021800160914     C                   KFLD                    VMSDDE
021900160914     C                   KFLD                    VMSCauTip
022000160914     C                   KFLD                    VMSCauBRT
022100160912     C*
022200160912     C                   ENDSR
022300160912     C*
022400140116     C*------------------------------------------------------------------------*
022500140116     C* Controlli sul membro che non fa il CLLE chiamante
022600140116     C*------------------------------------------------------------------------*
022700140116     C     CtrlMbr       BEGSR
022800140116     C*
022900140116     C* controllo che il membro in elaborazione non sia una copia di salvataggio
023000140116     C                   IF        %subst(pIn_MLNAME :
023100140116     C                               %checkr(' ' : pIn_MLNAME)) = '£'
023200140116     C                   RETURN
023300140116     C                   ENDIF
023400140116     C*
023500140116     C                   ENDSR
023600160912     C*
023700160912     C*------------------------------------------------------------------------*
023800160912     C* Scrivo il rcd
023900160912     C*------------------------------------------------------------------------*
024000160912     C     WriteVMS      BEGSR
024100160912     c*
024200160912     c* imposto i dati da scrivere che ho ricevuto
024300160912     c                   eval      F_VMSATB    = *blank
024400160912     c                   eval      F_VMSPROC   = pIn_Proc
024500160912     c                   eval      F_VMSKSU    = pIn_KSU
024600160912     c                   eval      F_VMSDDE    = pIn_DDE
024700160912     c                   eval      F_VMSDSC    = pIn_DSC
024800160912     c* imposto i dati da scrivere con quelli trovati nell'elemento di schiera
024900160912     c                   clear                   F_VMSCAUBRT
025000160912     c                   clear                   F_VMSSTSCLI
025100160912     c                   clear                   F_VMSSTSOPZ
025200160912     c                   clear                   F_VMSDESCRIZ
025300160912     c                   select
025400160912     c                   when      %trim(wIdSk) = %trim(pIn_Sk1)
025500160912     c                   select
025600160912     c                   when      pIn_TpSk = '1'
025700160912     c* CAUBRT mi serve anche per il test successivo
025800160912     c                   eval      VMSCAUBRT = DS_WCAU_1.dsCAU_BRT
025900160912     c                   eval      F_VMSCAUBRT = DS_WCAU_1.dsCAU_BRT
026000160912     c                   eval      F_VMSSTSCLI = DS_WCAU_1.dsCAU_CLI
026100160912     c                   eval      F_VMSSTSOPZ = DS_WCAU_1.dsCAU_OPZ
026200160912     c                   eval      F_VMSDESCRIZ= DS_WCAU_1.dsCAU_DES
026300160912     c                   when      pIn_TpSk = '2'
026400160912     c* CAUBRT mi serve anche per il test successivo
026500160912     c                   eval      VMSCAUBRT = DS_WCAU_2.dsCAU_BRT
026600160912     c                   eval      F_VMSCAUBRT = DS_WCAU_2.dsCAU_BRT
026700160912     c                   eval      F_VMSSTSCLI = DS_WCAU_2.dsCAU_EVT +
026800160912     c                                           DS_WCAU_2.dsCAU_RSN
026900160912     c                   eval      F_VMSDESCRIZ= DS_WCAU_2.dsCAU_DES
027000160912     c                   endsl
027100160912     c                   when      %trim(wIdSk) = %trim(pIn_Sk2)
027200160912     c                   select
027300160912     c                   when      pIn_TpSk = '1'
027400160912     c* CAUBRT mi serve anche per il test successivo
027500160912     c                   eval      VMSCAUBRT = DS_WDCT_1.dsDCT_BRT
027600160912     c                   eval      F_VMSCAUBRT = DS_WDCT_1.dsDCT_BRT
027700160912     c                   eval      F_VMSSTSCLI = DS_WDCT_1.dsDCT_CLI
027800160912     c                   when      pIn_TpSk = '2'
027900160912     c* CAUBRT mi serve anche per il test successivo
028000160912     c                   eval      VMSCAUBRT = DS_WDCT_2.dsDCT_BRT
028100160912     c                   eval      F_VMSCAUBRT = DS_WDCT_2.dsDCT_BRT
028200160912     c                   eval      F_VMSSTSCLI = DS_WDCT_2.dsDCT_EVT +
028300160912     c                                           DS_WDCT_2.dsDCT_RSN
028400160912     c                   endsl
028500160912     c                   when      %trim(wIdSk) = %trim(pIn_Sk3)
028600160912     c                   select
028700160912     c                   when      pIn_TpSk = '1'
028800160912     c* CAUBRT mi serve anche per il test successivo
028900160912     c                   eval      VMSCAUBRT = DS_WTIS9_1.dsTIS9_BRT
029000160912     c                   eval      F_VMSCAUBRT = DS_WTIS9_1.dsTIS9_BRT
029100160912     c                   eval      F_VMSSTSCLI = DS_WTIS9_1.dsTIS9_CLI
029200160912     c                   when      pIn_TpSk = '2'
029300160912     c* CAUBRT mi serve anche per il test successivo
029400160912     c                   eval      VMSCAUBRT = DS_WTIS9_2.dsTIS9_BRT
029500160912     c                   eval      F_VMSCAUBRT = DS_WTIS9_2.dsTIS9_BRT
029600160912     c                   eval      F_VMSSTSCLI = DS_WTIS9_2.dsTIS9_EVT +
029700160912     c                                           DS_WTIS9_2.dsTIS9_RSN
029800160912     c                   endsl
029900160912     c                   endsl
030000160914     C*
030100160914     c* se CAUBRT è vuoto, non proseguo
030200160914     c                   if        VMSCAUBRT <> *blank
030300160912     C*
030400160914     C* calcolo CAUTIP da CAUBRT
030500160914     C                   exsr      CauTipDaCauBRT
030600160914     C*
030700160914     C* prima di eseguire la write, devo recuperare i dati non presenti nell'elemento di schiera
030800160912     C* lo cerco nel rcd standard
030900160912     C                   eval      VMSDDE = 20010101
031000160912     C                   eval      VMSPROC = '*STD'
031100160912     C                   eval      VMSKSU = *blank
031200160912     c*
031300160914     c     K05VMS01      chain     TIVMS01
031400160912     c* se lo trovo e non è annullato, valorizzo i campi i campi ancora vuoti con quelli reperiti,
031500160912     c* altrimenti li lascio vuoti tranne la descrizione che ho recuperato dall'elemento di schiera
031600160912     c                   if        %found()
031700160912     c                             and VMSATB = *blank
031800160912     c                   eval      F_VMSCAUTIP = VMSCAUTIP
031900160912     c                   eval      F_VMSCAUTT  = VMSCAUTT
032000160912     c                   eval      F_VMSFLO    = VMSFLO
032100160912     c                   eval      F_VMSFASOP  = VMSFASOP
032200160912     c                   eval      F_VMSDESCRIZ= VMSDESCRIZ
032300160912     c                   eval      F_VMSDESCIT = VMSDESCIT
032400160912     c                   eval      F_VMSDESCEN = VMSDESCEN
032500160912     c                   eval      F_VMSDESCFR = VMSDESCFR
032600160912     c                   eval      F_VMSDESCDE = VMSDESCDE
032700160912     c                   eval      F_VMSDESCES = VMSDESCES
032800160912     c                   else
032900160914     c                   eval      F_VMSCAUTIP = *all'*'
033000160912     c                   clear                   F_VMSCAUTT
033100160912     c                   clear                   F_VMSFLO
033200160912     c                   clear                   F_VMSFASOP
033300160912     c                   clear                   F_VMSDESCIT
033400160912     c                   clear                   F_VMSDESCEN
033500160912     c                   clear                   F_VMSDESCFR
033600160912     c                   clear                   F_VMSDESCDE
033700160912     c                   clear                   F_VMSDESCES
033800160912     c                   endif
033900160912     C*
034000160912     c* se alla fine di tutto CAUBRT è vuoto, non scrivo
034100160912     c                   if        F_VMSCAUBRT <> *blank
034200160912     C*
034300160912     C* adesso scrivo
034400160912     C                   write     TIVMS000
034500160912     C*
034600160912     C* memorizzo che ho scritto
034700160912     C                   eval      wWrite=*on
034800160912     c                   endif
034900160914     C*
035000160914     c                   endif
035100160914     C*
035200160914     C                   ENDSR
035300160914     C*
035400160914     C*------------------------------------------------------------------------*
035500160914     C* Calcolo CAUTIP da CAUBRT
035600160914     C*------------------------------------------------------------------------*
035700160914     C     CauTipDaCauBRTBEGSR
035800160914     C*
035900160914     c                   select
036000160914
036100160914     c                   when      VMSCAUBRT = 'SPUNTNPG5'
036200160914     c                   eval      VMSCAUTIP = 'COL'
036300160914
036400160914     c                   when      %len(%trim(VMSCAUBRT)) > 4 and
036500160914     c                             %subst(%trim(VMSCAUBRT):1:4) = 'GIAC'
036600160914     c                   eval      VMSCAUTIP = 'GCP'
036700160914
036800160914     c                   when      %len(%trim(VMSCAUBRT)) = 1
036900160914     c                   eval      VMSCAUTIP = 'ICE'
037000160914
037100160914     c                   when      %len(%trim(VMSCAUBRT)) = 3
037200160914     c                   eval      VMSCAUTIP = 'ICE'
037300160914
037400160914     c                   when      %len(%trim(VMSCAUBRT)) = 2
037500160914     c                   monitor
037600160914     c                   if        %dec(%trim(VMSCAUBRT):10:0) > 0
037700160914     c                   eval      VMSCAUTIP = 'TAD'
037800160914     c                   endif
037900160914     c                   on-error
038000160914     c                   eval      VMSCAUTIP = 'ICE'
038100160914     c                   endmon
038200160914
038300160914     c                   other
038400160914     c                   eval      VMSCAUTIP = 'STS'
038500160914
038600160914     c                   endsl
038700160914
038800160912     C*
038900160912     C                   ENDSR
