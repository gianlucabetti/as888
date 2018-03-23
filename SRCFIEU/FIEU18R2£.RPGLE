000100991124     H DECEDIT('0,') DATEDIT(*DMY/)
000200160902      **?------------------------------------------------------------------ */
000300160902      *? Crea BOLLA di recupero da DIROTTAMENTO DPD
000400160902      **?------------------------------------------------------------------ */
000500160902     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
000600161014     FFiar401L  iF   E           K DISK    rename(FIAR4000:FIAR401)
000700170407     FFiar405L  iF   E           K DISK    infds(ar4ds)
000800161014     Ffiar400f  uF   E             DISK    rename(FIAR4000:FIAR4FIS)
000900161014     f                                     Prefix(Fis)
001000170307     FFiar501L  iF   E           K DISK
001100160909     FFiapd01L  iF   E           K DISK
001200160907     Ffibsp01l  iF   E           K DISK
001300160907     FTABEL00F  iF   E           K DISK
001400060615      **?------------------------------------------------------------------ */
001500160902      *? Ds Decodifica dei campi DS PASSATA COME PARAMETRO
001600060719      **?------------------------------------------------------------------ */
001700161130     D FIEU18        E DS                  EXTNAME(FIEU18DS)
001800160902     D FNARB00       E DS                  EXTNAME(FNARB00F)
001900161014     D*-------------
002000161014     d ar4ds           ds
002100161014     d  ar4_nrr              397    400b 0
002200161014     D*-------------
002300160907     D TISI95DS      E DS
002400170307     D Dar5GEN       E DS
002500161014     D DSbl4i        E DS
002600160902     D DTASV         E DS
002700160907     D Dblp          E DS
002800160907     D Dblp01        E DS
002900170324     d ParLnpB         s              3  0
003000160902     D trul90ds      E DS
003100060719      **?------------------------------------------------------------------ */
003200160901      *?  Parametri x Controllo profilo utenti  ?
003300160902      **?------------------------------------------------------------------ */
003400160901     d TIBS34ds      e ds
003500160902     d TIBS36ds      e ds
003600160901     d AZUTEds       e ds                  extname(AZUTE00F)
003700160901     d DDatiUte      e ds
003800160902     d TRULVPODS     e ds
003900160902     d   sk_PDA               16    765    inz   dim(250)
004000160905     D DS3A          E DS
004100160907     D OG150         E DS
004200160908     D                 DS
004300160908     D  TESTO3                 1      8
004400160908     D  RAAS                   9     10  0
004500160908     D  RLNP                  12     14  0
004600160908     D  RNRS                  16     17  0
004700160908     D  RNSP                  19     25  0
004800160908     D  RIFSPE                 1     25
004900160901     D*------------------
005000160901     D* DS REPEIMENTO TERMINALs
005100160901     D*------------------
005200160901     D FNLV55DS      E DS
005300160902     D KPJBA         E DS
005400160901      *---------------------
005500160907     D Wdataeur        S               D   DATFMT(*eur)
005600160907     D Wdataiso        S               D   DATFMT(*iso)
005700160907      *---------------------
005800160902     D Digits          C                   '0123456789'
005900160902      *----------------------------------------------------*
006000160902     IAZORG01L  AA
006100160902     I                                  1    3  ORGDIT
006200160902     I                                  6    6  ORGFVA
006300160902     I                                 11   11  ORGFL1
006400160902     I                                 14   33  ORGDES
006500160902     I                                 34   53  ORGIND
006600160902     I                             P   54   56 0ORGCPF
006700160902     I                                 57   76  ORGLOC
006800160902     I                                 77   78  ORGPRO
006900160902     I                               3776 3800  ORGDE3
007000160902     I                               3851 3875  ORGDE6
007100160902     I                               3901 3925  ORGDE8
007200160902     I                               3951 3975  ORGDF0
007300911213      *----------------------------------------------------*
007400060615     c     *ENTRY        PLIST
007500161130     c                   parm                    FIEU18
007600160902     c                   parm                    fnarb00
007700160902     c                   parm                    KPJBA
007800160902      *
007900160902     C                   eval      *inrt = *on
008000161017      *
008100161017      * Deve eseguire un solo addebito al cliente per recuperare
008200161017     c                   eval      AR4TRC ='I'
008300161017     c                   eval      AR4N14 = I5parcel
008400170407     c     kar4I         chain     fiar405L
008500170407     c                   if        %Found(fiar405L)
008600161017     c                   eval      dsbl4i =ar4not
008700161017     c                   if        §B4recDIR <>*blank
008800161017     c                   RETURN
008900161017     c                   end
009000161017     c                   endif
009100170307     C*
009200170307     C*  Deve saltare le bolle che sono state da EEX messe su DPD (vedi rec.GEN)
009300170307     C                   move      'GEN'         §GEN              3
009400170307     c     kar5          chain     fiar501L
009500170307     c                   if        %Found(Fiar501L)
009600170307     c                   eval      dar5GEN=AR5UNI
009700170307     c                   if        §AR5FDPD ='S'
009800170307     c                   RETURN
009900170307     c                   end
010000170307     c                   end
010100170307     C*
010200160905     C     KTAB          KLIST
010300160905     C                   KFLD                    tblKUT
010400160905     C                   KFLD                    tblCOD
010500160905     C                   KFLD                    tblKEY
010600170307     C*
010700170307     C     KAR5          KLIST
010800170307     C                   KFLD                    ARBAAS
010900170307     C                   KFLD                    ARBLNP
011000170307     C                   KFLD                    ARBNRS
011100170307     C                   KFLD                    ARBNSP
011200170307     C                   KFLD                    §GEN              3
011300160907     C*
011400160907     C     KAR4          KLIST
011500160907     C                   KFLD                    ARBAAS
011600160907     C                   KFLD                    ARBLNP
011700160907     C                   KFLD                    ARBNRS
011800160907     C                   KFLD                    ARBNSP
011900160907     C                   KFLD                    §TRC              1
012000161014     C*
012100161014     C     KAR4i         KLIST
012200161014     C                   KFLD                    AR4trc
012300161014     C                   KFLD                    AR4n14
012400160905     C*
012500160905     C                   Z-ADD     1             tblKUT
012600160909     C*
012700160909     C     Kapd          KLIST
012800160909     C                   KFLD                    tipoaut
012900160909     C                   KFLD                    Autista
013000160909     C                   move      'A'           tipoaut           1
013100160901      *
013200160908      **?------------------------------------------------------------------ */
013300160908      **?  Parametri x Controllo profilo utenti                            ?
013400160908      **?------------------------------------------------------------------ */
013500160908      *
013600160901      *  prende i dati utente
013700160901     c                   exsr      DATIjob
013800160901      *
013900160902     c                   exsr      BOLLA_RECUPER
014000010219      *
014100160902     c                   RETURN
014200060719      **?------------------------------------------------------------------ */
014300060719      *   DEFINIZIONE CHIAVI
014400060719      **?------------------------------------------------------------------ */
014500940321     C     *INZSR        BEGSR
014600020924      *
014700020924      *  Decodifica il profilo generico EDPCED in base alla LNA e quindi al Terminal
014800020924      *   EDPCEDxxx dove xxx è il Terminal della LNA
014900021114     C                   CLEAR                   Tibs36Ds
015000060707     c                   EVAL      I36ute = 'EDPCED'
015100021114     c                   EVAL      I36Tla = 'L'
015200021114     C                   CALL      'TIBS36R'
015300021114     C                   PARM                    Tibs36Ds
015400000908      *
015500971216      * Recupero data e ora
015600160907     C                   TIME                    WORA              6 0
015700160907     C                   TIME                    W0140            14 0
015800991124      * UDATE IN GGMMAAAA
015900160907     C                   MOVE      W0140         oggi              8 0
016000991124      * UDATE IN AAAAMMGG
016100160907     C                   MOVEL     oggi          Wdataeur
016200160907     C                   MOVEL     Wdataeur      Wdataiso
016300160907     C                   MOVEL     Wdataiso      datacor           8 0
016400010219      *
016500060616     C                   ENDSR
016600160902      **?------------------------------------------------------------------ */
016700160902      *?  Reperimento Dati del job (Utente/Operativi)                ?
016800160902      **?------------------------------------------------------------------ */
016900160902     c     DatiJOB       BEGSR
017000160902      *
017100160902     c     *dtaara       define    §azute        azuteds
017200160902     c     *dtaara       define    §datiute      ddatiute
017300160902      *
017400161005     c                   in(E)     *dtaara
017500160902     c                   IF        %ERROR or RSUT = *blanks
017600160902     c                   clear                   Tibs34Ds
017700160902     c                   call      'TIBS34R'
017800160902     c                   parm                    Tibs34Ds
017900160902     c                   in        *dtaara
018000160902     c                   ENDIF
018100160902      *
018200160902     C* REPERISCE IL TERMINAL PARTENZA x l'area dati 34DS
018300160902     C                   CLEAR                   FNLV55DS
018400160902     C                   MOVEL     'P'           D55TPT
018500160907     C                   MOVEL     arblna        D55LIN
018600160907     C                   Z-ADD     DATaCOR       D55DRF
018700160902     C                   CALL      'FNLV55R'
018800160902     C                   PARM                    FNLV55DS
018900160902IF  1C     D55ERR        IFNE      *BLANKS
019000160907     C                   MOVEL     arblna        D55TFP
019100160902E   1C                   ENDIF
019200160902     C*
019300160902      ** imposta il SIMFEL come terminal di partenza
019400160902      *   Aggiorno area dati Dati Utente
019500160902     C     *lock         in        DDatiUte
019600160902     c                   movel     d55tfp        simfel
019700160902     C                   out       DDatiUte
019800160902     C                   UnLock(e) DDatiUte
019900160902      *
020000160902     c                   ENDSR
020100060719      **?------------------------------------------------------------------ */
020200160831      *  GENERA BOLLA DI RECUPERO SPESE DI DIROTTAMENTO al cliente
020300160831      **?------------------------------------------------------------------ */
020400160831     C     BOLLA_RECUPER BEGSR
020500160901      *
020600161014     c                   move      'N'           ha_scritto        1
020700161025      *
020800161025     c***   per non codificato esce dalla routine senza fare alcuna bolla
020900161025     c                   move      arbksc        fld8888           4
021000161025     c                   if        fld8888 ='8888'
021100161025     c                   leavesr
021200161025     c                   end
021300160901      *
021400160902     C                   clear                   DTASV
021500160902     C                   clear                   trul90ds
021600160901     c                   clear                   dblp
021700160906      *
021800160906     c                   eval      §lpnpr = I5PARCEL
021900160906     c                   eval      §lpfpr='D'
022000160906     c                   move      'F*'          §lpcbo
022100160906     C                   MOVEL     '*'           §LPSOP
022200160901      *
022300160901     c                   clear                   dblp01
022400160907     c                   movel     I5SERVICE     §LPscd
022500160909     c                   movel     *blank        §lptrd
022600160901     c                   movel     dblp01        §lpflo
022700160905      *
022800160905      *  Inverto mittente e destinatario
022900160905     C                   MOVEL     ARBRSD        §LPRSM
023000160905     C                   MOVEL     ARBIND        §LPINM
023100160905     C                   MOVEL     ARBCAD        §LPCAM
023200160905     C                   MOVEL     ARBLOD        §LPLOM
023300160905     C                   MOVEL     ARBPRD        §LPPRM
023400160905     C                   MOVEL     ARBNZD        §LPNZM
023500160905     C                   MOVEL     ARBRSM        §LPRSD
023600160905     C                   MOVEL     ARBINM        §LPIND
023700160905     C                   MOVEL     ARBCAM        §LPCAD
023800160905     C                   MOVEL     ARBLOM        §LPLOD
023900160905     C                   MOVEL     ARBPRM        §LPPRD
024000160905     C                   MOVEL     ARBNZM        §LPNZD
024100160905     C                   MOVEL     ARBLNP        §LPLNA
024200160905     C                   MOVEL     ARBLNA        §LPLNP
024300160905     C                   MOVE      ARBCCM        §LPCCM
024400160905     C                   MOVEL     §LPCCM        §LIN              3 0
024500160907     C                   MOVE      §LPCCM        Campo4            4 0
024600160905     C     §LIN          IFNE      §LPLNP
024700160907     C     Campo4        ANDEQ     8888
024800160905     C                   MOVEL     §LPLNP        §LPCCM
024900160905     C                   END
025000160905     C*
025100160905     c                   movel     arblna        §lpccn
025200160905     c                   move      9999          §lpccn
025300160905     C*
025400160905     C                   MOVEL     DATACOR       §LPAAS
025500160905     C                   MOVE      DATACOR       §LPMGS
025600160905     C                   MOVE      ARBRMN        §LPRMN
025700160905     C                   MOVEL     ARBRMA        §LPRMA
025800160905     C                   MOVEL     ARBRMO        §LPRMO
025900160905     C                   MOVE      datacor       §LPDRT
026000160905     C                   MOVEL     'P'           §LPFPP
026100160909     c                   movel     arblna        autista           7 0
026200160909     c                   move      '0998'        autista
026300160909     c                   z-add     autista       §lppdr
026400160909     c     kapd          chain     fiapd01l
026500160909     c                   IF        not %Found(Fiapd01l)
026600160909      *   L'Autista
026700160907     c* il padroncino è da prendere da tb bsp: altrimenti imposto fisso 999
026800160907     c     arblna        chain     fibsp01l
026900160907     c                   if        %found(fibsp01l)
027000160907     c                   z-add     bspPDR        §lppdr
027100160907     c                   else
027200160907     c                   z-add     999           §lppdr
027300160907     c                   end
027400160909     c                   endIF
027500160909      *  Provenienza
027600160909     c                   move      *all'9'       §lppoe
027700160909     c                   move      *all'9'       §lpnsr
027800160909     c                   move      *all'9'       §lpnor
027900160909     c                   move      *all'9'       §lpnrv
028000160907      *
028100160905     C                   MOVEL     'G'           §LPRIC
028200160906     C                   MOVE      ARBNAS        §LPNAS
028300160906     C*
028400160905     C                   MOVE      ARBNCL        §LPNCL
028500160905     C                   MOVE      ARBPKF        §LPPKB
028600160905     C                   MOVE      ARBPKC        §LPPKC
028700160905     C                   MOVE      ARBNCP        §LPNCP
028800160905     C                   MOVE      ARBVLF        §LPVLB
028900160905     C                   MOVE      ARBVLC        §LPVLC
029000160905     C                   MOVE      ARBNCR        §LPNCR
029100160906      ***
029200160905     C                   MOVE      ARBTSP        §LPTSP
029300160905      * forzatura h 10,30 x dirottamenti resi viene trasformato in 'E'
029400160905     c                   if        §lptsp = 'H'
029500160905     c                   move      'E'           §lptsp
029600160905     c                   endif
029700160905      *
029800160905     C* reperisco variabili tipo bolla
029900160905     C                   CLEAR                   DS3A
030000160905     C                   MOVEL     '3A'          TBLCOD
030100160905     C                   MOVEL     *BLANK        TBLKEY
030200160905     C                   MOVEL     §lpCBO        TBLKEY
030300160907     C     KTAB          CHAIN     TABEL00F                           21
030400160905     C  N21              MOVEL     TBLUNI        DS3A
030500160905      *
030600160905     C     ARBFBR        IFNE      *BLANK
030700160905     C     §3asva        andeq     *blank
030800160905     C                   MOVE      ARBFBR        §LPFBR
030900160905     C                   END
031000160905     C*  Se bolla originale in franco frontiera olla figlia no
031100160905     C     ARBFBR        IFEQ      'F'
031200160905     C                   MOVEL     *BLANKS       §LPFBR
031300160905     C                   END
031400160905      **
031500160905     C                   MOVEL     §3ATB1        WNEWPO            1
031600160905     C     WNEWPO        IFEQ      'A'
031700160905     C     §3ATB1        ANDNE     'AS'
031800160907     C     Campo4        ANDEQ     8888
031900160905     C                   MOVE      '9999'        §LPCCM
032000160905     C                   MOVE      '9999'        §LPCCN
032100160905     C                   END
032200160905     C*
032300160905     C* SE CODICE MITTENTE = 8888/9999        PASSO 0 NEL CODICE
032400160905     C* TARIFFA ALTRIMENTI GLI PASSO IL CODICE DELLA BOLLA ORIG.
032500160905     C                   MOVE      §LPCCM        W8899             4 0
032600160905     C     W8899         IFne      8888
032700160905     C     W8899         ANDne     9999
032800160905     C                   MOVE      ARBCTR        §LPCTR
032900160905     C                   END
033000160905      *
033100160831      *  Calcolo zona consegna
033200160907     C                   CLEAR                   TISI95ds
033300160831     C                   MOVEL     '3'           I95TCN
033400160831     C                   MOVEL     *BLANK        I95TLA
033500160902     C                   MOVEL     §LPNZD        I95NAR
033600160902     C                   MOVEL     §LPIND        I95IND
033700160902     C                   MOVEL     §LPCAD        I95CAP
033800160902     C                   MOVEL     §LPLOD        I95LOC
033900160902     C                   MOVEL     §LPPRD        I95PRV
034000160831     C                   MOVEL     ARBPKF        I95LKG
034100160831     C                   MOVEL     ARBVLF        I95LMC
034200160831     C                   MOVEL     ARBFFD        I95FFD
034300160831     C                   MOVEL     ARBTSP        I95TSP
034400160901     C                   MOVEL     'D'           I95fi1
034500160831     C                   MOVEL     ARBtc1        I95fi2
034600160831     C                   MOVE      ARBtc2        I95fi2
034700160831     C                   CALL      'TISI95R'
034800160907     C                   PARM                    TISI95ds
034900160831     C                   MOVEL     O95ZNC        §LPZNC
035000160831     C*
035100160908     C* FORZO CTM x eseguire un RECUPERO e deve essere forzatamente un "4F"
035200160908     C*  altrimenti la LNA non viene presa in considerazione dalla bollettazione
035300160908     C                   MOVEL     '4F'          §LPCTM
035400160907     c*** ????????
035500160905     C*
035600160902     C* Reperisco flag x stampare o meno bolla in partenza
035700160902      * a meno che non abbia già fatto un tentativo nel caso forzo sempre'N'
035800160902     c                   if        arbntc > 0
035900160902     c                   move      'N'           §lpbam
036000160902     c                   else
036100160902     C                   CLEAR                   AR4NOT
036200160907     C                   MOVE      'A'           §TRC              1
036300160902     C     KAR4          CHAIN(N)  FiAR401L                           43
036400160902     C  N43              MOVEL     AR4NOT        §LPBAM
036500160902     C   43              MOVEL     'S'           §LPBAM
036600160902     c                   endif
036700160902      *
036800160902     C*--------------------------------------------------------*
036900160905     C                   MOVE      ARBFVF        §LPFVB
037000160905      *
037100160902     C*  Se il flag del volume bollettato è  'C' o 'K' oppure
037200160902     C*  è  'P' ed il cliente è codificato imposto 'R' in modo
037300160902     C*  che non venga più ricalcolato
037400160902     C     ARBFVF        IFEQ      'C'
037500160902     C     ARBFVF        OREQ      'K'
037600160902     C     ARBFVF        OREQ      'P'
037700160907     C     Campo4        ANDEQ     8888
037800160902     C     ARBFVF        OREQ      'P'
037900160907     C     Campo4        ANDEQ     9999
038000160902     C                   MOVEL     'R'           §LPFVB
038100160902     C                   END
038200160902     C*--------------------------------------------------------*
038300160902     c*
038400160902     c                   clear                   §lpksca
038500160902     c                   clear                   §lpctra
038600160908     c                   move      arbksc        §lpksca
038700160908     c                   move      arbctr        §lpctra
038800160908     c                   z-add     arbaas        §LPaasM
038900160908     c                   z-add     arblnp        §LPlnpM
039000160908     c                   z-add     arbnrs        §LPnrsM
039100160908     c                   z-add     arbnsp        §LPnspM
039200160908     c*   NOTE
039300160908     C                   clear                   §LPNot
039400160908     C                   clear                   §LPNT2
039500160908     c                   exsr      oldnot
039600160908     C*
039700160908      *imposto i riferimenti EX-SPED solo se non è dirottamento
039800160908     C                   MOVEL     'SPED.:'      TESTO3
039900160908     C                   MOVE      arbAAS        RAAS
040000160908     C                   MOVE      arbLNP        RLNP
040100160908     C                   MOVE      arbNRS        RNRS
040200160908     C                   MOVE      arbNSP        RNSP
040300160908     C                   MOVEL     RIFSPE        §LPNT2
040400160902      ******
040500170324     c                   eval      knmus = 'EDPCEDXXX'
040600170406     c                   eval      knsif = 'FILTRA201'
040700170324     C                   z-add     §LPLNP        ParLnpB
040800160902     C                   CALL      'FNLS01R'
040900160902     C                   PARM                    KPJBA
041000160902     C                   PARM                    DBLP
041100160902     C                   PARM                    DTASV
041200160902     C                   PARM                    trul90ds
041300170324     C                   PARM                    ParLnpB
041400160902      *
041500160902      *  Se non c'è riuscito a scrivere la bolla
041600161014     c                   if        §LPFPR <>'5'
041700161014     c                   move      'S'           ha_scritto
041800161014     c                   eval      AR4TRC ='I'
041900161014     c                   eval      AR4N14 = I5parcel
042000170407     c     kar4I         chain     fiar405L
042100170407     c                   if        %Found(fiar405L)
042200161014      **  aggancia il Fisico
042300161014     c     ar4_nrr       chain     fiar400F
042400161014     c                   if        %Found(fiar400F)
042500161014     c                   eval      dsbl4i    = FISar4not
042600161017     c                   eval      §B4recDIR = 'D'
042700161014     c                   eval      FISar4not = dsbl4i
042800161014     c                   update    fiar4FIS
042900161014     c                   end
043000161014     c                   end
043100161014     c                   end
043200160831      *
043300160831     C                   ENDSR
043400160902      **?------------------------------------------------------------------ */
043500160908     C* O l d N o t - reperimento note da vecchia bolla
043600160908      **?------------------------------------------------------------------ */
043700160908     C     oldnot        BEGSR
043800160908      *
043900160908     c                   clear                   campo_70         70
044000160908     C                   MOVE      '8'           §TRC
044100160908     C     KAR4          CHAIN(N)  FiAR401L
044200160908     c                   if        %found(fiar401l)
044300160908     C                   eval      campo_70 = %trim(ar4not)
044400160908     c                   endif
044500170407      ****  solo la prima parte di 35 caratteri della bolla mamma
044600160908     C                   movel     campo_70      §lpNot
044700160908
044800160908     C                   ENDSR
044900160908     C*-----------------------------------------------------
