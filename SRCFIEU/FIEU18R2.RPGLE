000100991124     H DECEDIT('0,') DATEDIT(*DMY/)
000200160902      **?------------------------------------------------------------------ */
000300160902      *? Crea BOLLA di recupero da DIROTTAMENTO DPD
000400160902      **?------------------------------------------------------------------ */
000500160902     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
000600161014     FFiar401L  iF   E           K DISK    rename(FIAR4000:FIAR401)
000700170925      *--
000800170925     FFiPND03L  uF   E           K DISK
000900170307     FFiar501L  iF   E           K DISK
001000160909     FFiapd01L  iF   E           K DISK
001100160907     Ffibsp01l  iF   E           K DISK
001200160907     FTABEL00F  iF   E           K DISK
001300060615      **?------------------------------------------------------------------ */
001400160902      *? Ds Decodifica dei campi DS PASSATA COME PARAMETRO
001500060719      **?------------------------------------------------------------------ */
001600161130     D FIEU18        E DS                  EXTNAME(FIEU18DS)
001700160902     D FNARB00       E DS                  EXTNAME(FNARB00F)
001800170927     d tibs55ds      E DS
001900161014     D*-------------
002000160907     D TISI95DS      E DS
002100170307     D Dar5GEN       E DS
002200170922     D DpndFLO       E DS
002300160902     D DTASV         E DS
002400160907     D Dblp          E DS
002500160907     D Dblp01        E DS
002600170324     d ParLnpB         s              3  0
002700160902     D trul90ds      E DS
002800060719      **?------------------------------------------------------------------ */
002900160901      *?  Parametri x Controllo profilo utenti  ?
003000160902      **?------------------------------------------------------------------ */
003100160901     d TIBS34ds      e ds
003200160902     d TIBS36ds      e ds
003300160901     d AZUTEds       e ds                  extname(AZUTE00F)
003400160901     d DDatiUte      e ds
003500160902     d TRULVPODS     e ds
003600160902     d   sk_PDA               16    765    inz   dim(250)
003700160905     D DS3A          E DS
003800160907     D OG150         E DS
003900160908     D                 DS
004000160908     D  TESTO3                 1      8
004100160908     D  RAAS                   9     10  0
004200160908     D  RLNP                  12     14  0
004300160908     D  RNRS                  16     17  0
004400160908     D  RNSP                  19     25  0
004500160908     D  RIFSPE                 1     25
004600160901     D*------------------
004700160901     D* DS REPEIMENTO TERMINALs
004800160901     D*------------------
004900160901     D FNLV55DS      E DS
005000160902     D KPJBA         E DS
005100160901      *---------------------
005200160907     D Wdataeur        S               D   DATFMT(*eur)
005300160907     D Wdataiso        S               D   DATFMT(*iso)
005400160907      *---------------------
005500160902     D Digits          C                   '0123456789'
005600160902      *----------------------------------------------------*
005700160902     IAZORG01L  AA
005800160902     I                                  1    3  ORGDIT
005900160902     I                                  6    6  ORGFVA
006000160902     I                                 11   11  ORGFL1
006100160902     I                                 14   33  ORGDES
006200160902     I                                 34   53  ORGIND
006300160902     I                             P   54   56 0ORGCPF
006400160902     I                                 57   76  ORGLOC
006500160902     I                                 77   78  ORGPRO
006600160902     I                               3776 3800  ORGDE3
006700160902     I                               3851 3875  ORGDE6
006800160902     I                               3901 3925  ORGDE8
006900160902     I                               3951 3975  ORGDF0
007000911213      *----------------------------------------------------*
007100060615     c     *ENTRY        PLIST
007200161130     c                   parm                    FIEU18
007300160902     c                   parm                    fnarb00
007400160902     c                   parm                    KPJBA
007500160902      *
007600160902     C                   eval      *inrt = *on
007700161017      *
007800170922     C* sostituito da ottobre 2017 con FIPND01L il flag di DIROTTAMENTO
007900170925      * aggancio con PARCEL
008000170925     c     I5parcel      chain(n)  fiPND03L
008100170925     c                   if        %Found(fipnd03L)
008200170922     c                   eval      dPNDFLO = pndFLO
008300170922     c                   if        §PNDRECDIR <> *blank
008400170922     c                   RETURN
008500170922     c                   end
008600170922     c                   endif
008700170922     C*
008800170307     C*  Deve saltare le bolle che sono state da EEX messe su DPD (vedi rec.GEN)
008900170307     C                   move      'GEN'         §GEN              3
009000170307     c     kar5          chain     fiar501L
009100170307     c                   if        %Found(Fiar501L)
009200170307     c                   eval      dar5GEN=AR5UNI
009300170307     c                   if        §AR5FDPD ='S'
009400170307     c                   RETURN
009500170307     c                   end
009600170307     c                   end
009700170307     C*
009800160905     C     KTAB          KLIST
009900160905     C                   KFLD                    tblKUT
010000160905     C                   KFLD                    tblCOD
010100160905     C                   KFLD                    tblKEY
010200170307     C*
010300170307     C     KAR5          KLIST
010400170307     C                   KFLD                    ARBAAS
010500170307     C                   KFLD                    ARBLNP
010600170307     C                   KFLD                    ARBNRS
010700170307     C                   KFLD                    ARBNSP
010800170307     C                   KFLD                    §GEN              3
010900160907     C*
011000160907     C     KAR4          KLIST
011100160907     C                   KFLD                    ARBAAS
011200160907     C                   KFLD                    ARBLNP
011300160907     C                   KFLD                    ARBNRS
011400160907     C                   KFLD                    ARBNSP
011500160907     C                   KFLD                    §TRC              1
011600160905     C*
011700160905     C                   Z-ADD     1             tblKUT
011800160909     C*
011900160909     C     Kapd          KLIST
012000160909     C                   KFLD                    tipoaut
012100160909     C                   KFLD                    Autista
012200160909     C                   move      'A'           tipoaut           1
012300160901      *
012400160908      **?------------------------------------------------------------------ */
012500160908      **?  Parametri x Controllo profilo utenti                            ?
012600160908      **?------------------------------------------------------------------ */
012700160908      *
012800160901      *  prende i dati utente
012900160901     c                   exsr      DATIjob
013000160901      *
013100160902     c                   exsr      BOLLA_RECUPER
013200010219      *
013300160902     c                   RETURN
013400060719      **?------------------------------------------------------------------ */
013500060719      *   DEFINIZIONE CHIAVI
013600060719      **?------------------------------------------------------------------ */
013700940321     C     *INZSR        BEGSR
013800020924      *
013900020924      *  Decodifica il profilo generico EDPCED in base alla LNA e quindi al Terminal
014000020924      *   EDPCEDxxx dove xxx è il Terminal della LNA
014100021114     C                   CLEAR                   Tibs36Ds
014200060707     c                   EVAL      I36ute = 'EDPCED'
014300021114     c                   EVAL      I36Tla = 'L'
014400021114     C                   CALL      'TIBS36R'
014500021114     C                   PARM                    Tibs36Ds
014600000908      *
014700971216      * Recupero data e ora
014800160907     C                   TIME                    WORA              6 0
014900160907     C                   TIME                    W0140            14 0
015000991124      * UDATE IN GGMMAAAA
015100160907     C                   MOVE      W0140         oggi              8 0
015200991124      * UDATE IN AAAAMMGG
015300160907     C                   MOVEL     oggi          Wdataeur
015400160907     C                   MOVEL     Wdataeur      Wdataiso
015500160907     C                   MOVEL     Wdataiso      datacor           8 0
015600010219      *
015700060616     C                   ENDSR
015800160902      **?------------------------------------------------------------------ */
015900160902      *?  Reperimento Dati del job (Utente/Operativi)                ?
016000160902      **?------------------------------------------------------------------ */
016100160902     c     DatiJOB       BEGSR
016200160902      *
016300160902     c     *dtaara       define    §azute        azuteds
016400160902     c     *dtaara       define    §datiute      ddatiute
016500160902      *
016600161005     c                   in(E)     *dtaara
016700160902     c                   IF        %ERROR or RSUT = *blanks
016800160902     c                   clear                   Tibs34Ds
016900160902     c                   call      'TIBS34R'
017000160902     c                   parm                    Tibs34Ds
017100160902     c                   in        *dtaara
017200160902     c                   ENDIF
017300160902      *
017400160902     C* REPERISCE IL TERMINAL PARTENZA x l'area dati 34DS
017500160902     C                   CLEAR                   FNLV55DS
017600160902     C                   MOVEL     'P'           D55TPT
017700160907     C                   MOVEL     arblna        D55LIN
017800160907     C                   Z-ADD     DATaCOR       D55DRF
017900160902     C                   CALL      'FNLV55R'
018000160902     C                   PARM                    FNLV55DS
018100160902IF  1C     D55ERR        IFNE      *BLANKS
018200160907     C                   MOVEL     arblna        D55TFP
018300160902E   1C                   ENDIF
018400160902     C*
018500160902      ** imposta il SIMFEL come terminal di partenza
018600160902      *   Aggiorno area dati Dati Utente
018700160902     C     *lock         in        DDatiUte
018800160902     c                   movel     d55tfp        simfel
018900160902     C                   out       DDatiUte
019000160902     C                   UnLock(e) DDatiUte
019100160902      *
019200160902     c                   ENDSR
019300060719      **?------------------------------------------------------------------ */
019400160831      *  GENERA BOLLA DI RECUPERO SPESE DI DIROTTAMENTO al cliente
019500160831      **?------------------------------------------------------------------ */
019600160831     C     BOLLA_RECUPER BEGSR
019700160901      *
019800161014     c                   move      'N'           ha_scritto        1
019900161025      *
020000161025     c***   per non codificato esce dalla routine senza fare alcuna bolla
020100161025     c                   move      arbksc        fld8888           4
020200161025     c                   if        fld8888 ='8888'
020300161025     c                   leavesr
020400161025     c                   end
020500160901      *
020600160902     C                   clear                   DTASV
020700160902     C                   clear                   trul90ds
020800160901     c                   clear                   dblp
020900160906      *
021000160906     c                   eval      §lpnpr = I5PARCEL
021100160906     c                   eval      §lpfpr='D'
021200160906     c                   move      'F*'          §lpcbo
021300160906     C                   MOVEL     '*'           §LPSOP
021400160901      *
021500160901     c                   clear                   dblp01
021600160907     c                   movel     I5SERVICE     §LPscd
021700160909     c                   movel     *blank        §lptrd
021800160901     c                   movel     dblp01        §lpflo
021900160905      *
022000160905      *  Inverto mittente e destinatario
022100160905     C                   MOVEL     ARBRSD        §LPRSM
022200160905     C                   MOVEL     ARBIND        §LPINM
022300160905     C                   MOVEL     ARBCAD        §LPCAM
022400160905     C                   MOVEL     ARBLOD        §LPLOM
022500160905     C                   MOVEL     ARBPRD        §LPPRM
022600160905     C                   MOVEL     ARBNZD        §LPNZM
022700160905     C                   MOVEL     ARBRSM        §LPRSD
022800160905     C                   MOVEL     ARBINM        §LPIND
022900160905     C                   MOVEL     ARBCAM        §LPCAD
023000160905     C                   MOVEL     ARBLOM        §LPLOD
023100160905     C                   MOVEL     ARBPRM        §LPPRD
023200160905     C                   MOVEL     ARBNZM        §LPNZD
023300160905     C                   MOVEL     ARBLNP        §LPLNA
023400160905     C                   MOVEL     ARBLNA        §LPLNP
023500160905     C                   MOVE      ARBCCM        §LPCCM
023600160905     C                   MOVEL     §LPCCM        §LIN              3 0
023700160907     C                   MOVE      §LPCCM        Campo4            4 0
023800160905     C     §LIN          IFNE      §LPLNP
023900160907     C     Campo4        ANDEQ     8888
024000160905     C                   MOVEL     §LPLNP        §LPCCM
024100160905     C                   END
024200160905     C*
024300160905     c                   movel     arblna        §lpccn
024400160905     c                   move      9999          §lpccn
024500160905     C*
024600160905     C                   MOVEL     DATACOR       §LPAAS
024700160905     C                   MOVE      DATACOR       §LPMGS
024800160905     C                   MOVE      ARBRMN        §LPRMN
024900160905     C                   MOVEL     ARBRMA        §LPRMA
025000160905     C                   MOVEL     ARBRMO        §LPRMO
025100160905     C                   MOVE      datacor       §LPDRT
025200160905     C                   MOVEL     'P'           §LPFPP
025300160909     c                   movel     arblna        autista           7 0
025400160909     c                   move      '0998'        autista
025500160909     c                   z-add     autista       §lppdr
025600160909     c     kapd          chain     fiapd01l
025700160909     c                   IF        not %Found(Fiapd01l)
025800160909      *   L'Autista
025900160907     c* il padroncino è da prendere da tb bsp: altrimenti imposto fisso 999
026000160907     c     arblna        chain     fibsp01l
026100160907     c                   if        %found(fibsp01l)
026200160907     c                   z-add     bspPDR        §lppdr
026300160907     c                   else
026400160907     c                   z-add     999           §lppdr
026500160907     c                   end
026600160909     c                   endIF
026700160909      *  Provenienza
026800160909     c                   move      *all'9'       §lppoe
026900160909     c                   move      *all'9'       §lpnsr
027000160909     c                   move      *all'9'       §lpnor
027100160909     c                   move      *all'9'       §lpnrv
027200160907      *
027300160905     C                   MOVEL     'G'           §LPRIC
027400160906     C                   MOVE      ARBNAS        §LPNAS
027500160906     C*
027600160905     C                   MOVE      ARBNCL        §LPNCL
027700160905     C                   MOVE      ARBPKF        §LPPKB
027800160905     C                   MOVE      ARBPKC        §LPPKC
027900160905     C                   MOVE      ARBNCP        §LPNCP
028000160905     C                   MOVE      ARBVLF        §LPVLB
028100160905     C                   MOVE      ARBVLC        §LPVLC
028200160905     C                   MOVE      ARBNCR        §LPNCR
028300160906      ***
028400160905     C                   MOVE      ARBTSP        §LPTSP
028500160905      * forzatura h 10,30 x dirottamenti resi viene trasformato in 'E'
028600160905     c                   if        §lptsp = 'H'
028700160905     c                   move      'E'           §lptsp
028800160905     c                   endif
028900160905      *
029000160905     C* reperisco variabili tipo bolla
029100160905     C                   CLEAR                   DS3A
029200160905     C                   MOVEL     '3A'          TBLCOD
029300160905     C                   MOVEL     *BLANK        TBLKEY
029400160905     C                   MOVEL     §lpCBO        TBLKEY
029500160907     C     KTAB          CHAIN     TABEL00F                           21
029600160905     C  N21              MOVEL     TBLUNI        DS3A
029700160905      *
029800160905     C     ARBFBR        IFNE      *BLANK
029900160905     C     §3asva        andeq     *blank
030000160905     C                   MOVE      ARBFBR        §LPFBR
030100160905     C                   END
030200160905     C*  Se bolla originale in franco frontiera olla figlia no
030300160905     C     ARBFBR        IFEQ      'F'
030400160905     C                   MOVEL     *BLANKS       §LPFBR
030500160905     C                   END
030600160905      **
030700160905     C                   MOVEL     §3ATB1        WNEWPO            1
030800160905     C     WNEWPO        IFEQ      'A'
030900160905     C     §3ATB1        ANDNE     'AS'
031000160907     C     Campo4        ANDEQ     8888
031100160905     C                   MOVE      '9999'        §LPCCM
031200160905     C                   MOVE      '9999'        §LPCCN
031300160905     C                   END
031400160905     C*
031500160905     C* SE CODICE MITTENTE = 8888/9999        PASSO 0 NEL CODICE
031600160905     C* TARIFFA ALTRIMENTI GLI PASSO IL CODICE DELLA BOLLA ORIG.
031700160905     C                   MOVE      §LPCCM        W8899             4 0
031800160905     C     W8899         IFne      8888
031900160905     C     W8899         ANDne     9999
032000160905     C                   MOVE      ARBCTR        §LPCTR
032100160905     C                   END
032200160905      *
032300160831      *  Calcolo zona consegna
032400160907     C                   CLEAR                   TISI95ds
032500160831     C                   MOVEL     '3'           I95TCN
032600160831     C                   MOVEL     *BLANK        I95TLA
032700160902     C                   MOVEL     §LPNZD        I95NAR
032800160902     C                   MOVEL     §LPIND        I95IND
032900160902     C                   MOVEL     §LPCAD        I95CAP
033000160902     C                   MOVEL     §LPLOD        I95LOC
033100160902     C                   MOVEL     §LPPRD        I95PRV
033200160831     C                   MOVEL     ARBPKF        I95LKG
033300160831     C                   MOVEL     ARBVLF        I95LMC
033400160831     C                   MOVEL     ARBFFD        I95FFD
033500160831     C                   MOVEL     ARBTSP        I95TSP
033600160901     C                   MOVEL     'D'           I95fi1
033700160831     C                   MOVEL     ARBtc1        I95fi2
033800160831     C                   MOVE      ARBtc2        I95fi2
033900160831     C                   CALL      'TISI95R'
034000160907     C                   PARM                    TISI95ds
034100160831     C                   MOVEL     O95ZNC        §LPZNC
034200160831     C*
034300160908     C* FORZO CTM x eseguire un RECUPERO e deve essere forzatamente un "4F"
034400160908     C*  altrimenti la LNA non viene presa in considerazione dalla bollettazione
034500160908     C                   MOVEL     '4F'          §LPCTM
034600160907     c*** ????????
034700160905     C*
034800160902     C* Reperisco flag x stampare o meno bolla in partenza
034900160902      * a meno che non abbia già fatto un tentativo nel caso forzo sempre'N'
035000160902     c                   if        arbntc > 0
035100160902     c                   move      'N'           §lpbam
035200160902     c                   else
035300160902     C                   CLEAR                   AR4NOT
035400160907     C                   MOVE      'A'           §TRC              1
035500160902     C     KAR4          CHAIN(N)  FiAR401L                           43
035600160902     C  N43              MOVEL     AR4NOT        §LPBAM
035700160902     C   43              MOVEL     'S'           §LPBAM
035800160902     c                   endif
035900160902      *
036000160902     C*--------------------------------------------------------*
036100160905     C                   MOVE      ARBFVF        §LPFVB
036200160905      *
036300160902     C*  Se il flag del volume bollettato è  'C' o 'K' oppure
036400160902     C*  è  'P' ed il cliente è codificato imposto 'R' in modo
036500160902     C*  che non venga più ricalcolato
036600160902     C     ARBFVF        IFEQ      'C'
036700160902     C     ARBFVF        OREQ      'K'
036800160902     C     ARBFVF        OREQ      'P'
036900160907     C     Campo4        ANDEQ     8888
037000160902     C     ARBFVF        OREQ      'P'
037100160907     C     Campo4        ANDEQ     9999
037200160902     C                   MOVEL     'R'           §LPFVB
037300160902     C                   END
037400160902     C*--------------------------------------------------------*
037500160902     c*
037600160902     c                   clear                   §lpksca
037700160902     c                   clear                   §lpctra
037800160908     c                   move      arbksc        §lpksca
037900160908     c                   move      arbctr        §lpctra
038000160908     c                   z-add     arbaas        §LPaasM
038100160908     c                   z-add     arblnp        §LPlnpM
038200160908     c                   z-add     arbnrs        §LPnrsM
038300160908     c                   z-add     arbnsp        §LPnspM
038400160908     c*   NOTE
038500160908     C                   clear                   §LPNot
038600160908     C                   clear                   §LPNT2
038700160908     c                   exsr      oldnot
038800160908     C*
038900160908      *imposto i riferimenti EX-SPED solo se non è dirottamento
039000160908     C                   MOVEL     'SPED.:'      TESTO3
039100160908     C                   MOVE      arbAAS        RAAS
039200160908     C                   MOVE      arbLNP        RLNP
039300160908     C                   MOVE      arbNRS        RNRS
039400160908     C                   MOVE      arbNSP        RNSP
039500160908     C                   MOVEL     RIFSPE        §LPNT2
039600160902      ******
039700170324     c                   eval      knmus = 'EDPCEDXXX'
039800170927     c**********         eval      knsif = 'FILTRA201'
039900170927      *--
040000170927     C* Richiamo TIBS55R per conoscere S.Informativi di FILIALE
040100170927     C* in cui utilizzare il lavoro
040200170927     C                   CLEAR                   TIBS55ds
040300170927     C                   MOVEL     'L'           I50TLA
040400170927     C                   MOVEL     '046'         I50PPO
040500170927     C                   MOVEL     '001'         I50APO
040600170927     C                   CALL      'TIBS55R'
040700170927     C                   PARM                    TIBS55ds
040800170927     c                   eval      knsif = O50ASI
040900170927      *--
041000170324     C                   z-add     §LPLNP        ParLnpB
041100170927      *--
041200160902     C                   CALL      'FNLS01R'
041300160902     C                   PARM                    KPJBA
041400160902     C                   PARM                    DBLP
041500160902     C                   PARM                    DTASV
041600160902     C                   PARM                    trul90ds
041700170324     C                   PARM                    ParLnpB
041800160902      *
041900160902      *  Se non c'è riuscito a scrivere la bolla
042000161014     c                   if        §LPFPR <>'5'
042100161014     c                   move      'S'           ha_scritto
042200170925      ***
042300170925     C* sostituito da ottobre 2017 con FIPND01L il flag di DIROTTAMENTO
042400170925     c     i5PARCEL      chain     fiPND03L
042500170925     c                   if        %Found(fipnd03L)
042600170925     c                   eval      dPNDFLO = pndFLO
042700170925     c                   eval      §PNDRECDIR ='D'
042800170925     c                   eval      PNDFLO = DpndFLO
042900170925     c                   update    fiPND000
043000170925     c                   endif
043100170922      *
043200161014     c                   end
043300160831      *
043400160831     C                   ENDSR
043500160902      **?------------------------------------------------------------------ */
043600160908     C* O l d N o t - reperimento note da vecchia bolla
043700160908      **?------------------------------------------------------------------ */
043800160908     C     oldnot        BEGSR
043900160908      *
044000160908     c                   clear                   campo_70         70
044100160908     C                   MOVE      '8'           §TRC
044200160908     C     KAR4          CHAIN(N)  FiAR401L
044300160908     c                   if        %found(fiar401l)
044400160908     C                   eval      campo_70 = %trim(ar4not)
044500160908     c                   endif
044600170407      ****  solo la prima parte di 35 caratteri della bolla mamma
044700160908     C                   movel     campo_70      §lpNot
044800160908
044900160908     C                   ENDSR
045000160908     C*-----------------------------------------------------
