000100001006     H DECEDIT('0,') DATEDIT(*YMD.)
000200980521      ****************************************************************
000300980521      *  RIEPILOGO INDICATORI
000400981008      ****************************************************************
000500980521      * 28 - ERRORE GENERICO DSPF
000600010604      * 31 - GENERICO
000700980624      * 90 - riemissione videata
000800990317      ****************************************************************
000900010611     FAZORG01L  IF   E           K DISK    RENAME(AZORG:AZORG000)
001000010604     FTNTBE01L  UF A E           K DISK
001100160718     FTNTBA02D  CF   E             WORKSTN
001200980521      *------------------------------------------------------------------------*
001300010604     D KCOD            S                   LIKE(TBECOD)
001400010604     D KKE1            S                   LIKE(TBEKE1)
001500980521      *   S C H I E R E
001600160718     D MSG             S             78    DIM(3) CTDATA PERRCD(1)              MSG VIDEO
001700010608     D datoggi         S              8  0
001800010608
001900010608     D W0140           S             14  0
002000010608     D Wdtgio          S              8  0
002100010608     D dateu           S              8  0
002200980521      *   D S   I N T E R N E / E S T E R N E
002300001009     D WLBDAT          DS                  INZ
002400001009     D  G02DAT                 1      8  0
002500001009     D  G02INV                 9     16  0
002600001009     D  G02ERR                17     17
002700001009     D  G02TGI                18     22  0
002800010308
002900001016     D TIBS02DS      E DS
003000980529     D UT§DSE0F      E DS
003100980601     D CNCR80        E DS
003200160718     D Dmau          E DS
003300980521      *
003400980521     D KPJBA         E DS
003500010608      *
003600001006      *------------------------------------------------------------------------*
003700001006     C                   do        *hival
003800001009      * Pulizia 1° Videata
003900010601     C                   if        (*in28 = *off and *in90 = *off)
004000160718     C                   MOVE      *BLANKS       V1Cmau
004100160718     C                   MOVE      *BLANKS       V1cmaud
004200160718     C                   MOVE      '?'           V1Cmau
004300001009     C                   endif
004400001011
004500010608      * Emissione 1° Videata
004600160718     C                   exfmt     TNTB011
004700001006      * F3=Fine
004800001006     C   kc              leave
004900010405
005000001006      * Controlli 1° videata
005100010604     C                   SETOFF                                       28
005200001006     C                   exsr      Sr_Contr01
005300980623
005400010601     C   28              iter
005500001009     C   90              iter
005600001009
005700010608      * Caricamento e pulizia seconda videata
005800010608     C                   exsr      Sr_carvid
005900010608
006000010608      * Emissione 2° Videata
006100010608     C     riemetto      tag
006200160718     C                   exfmt     TNTB012
006300010611
006400010611     C                   eval      *in28 = *off
006500010608
006600010606      * F3=Fine
006700010606     C   kc              LEAVE
006800010606
006900010608      * F12=Ritorno
007000010608     C   kl              ITER
007100010608
007200010608     C                   exsr      Sr_Contr02
007300010608     C   28              goto      riemetto
007400010608
007500010608      * F6=Aggiornamento
007600010608     C   kf              exsr      Sr_Registra
007700010608     C   kf              iter
007800010608
007900010608     C                   goto      riemetto
008000010608
008100001006     C                   enddo
008200981204
008300001006     C                   eval      *inlr = *on
008400980521      **********************************************************************
008500980521      * CONTROLLI VIDEO 1
008600980521      **********************************************************************
008700001006     C     Sr_Contr01    begsr
008800001009      * Reset indicatori
008900010612     C                   setoff                                       90
009000010601
009100160718     C     '?'           scan      v1cmau                                 31
009200010601     C     *in31         ifeq      *on
009300010601     C                   eval      *in90 = *on
009400010601     C                   clear                   TIBS02DS
009500010601     C                   movel     'R'           t02mod
009600010601     C                   movel     knsif         t02sif
009700160718     C                   movel     'MAU'         t02cod
009800010601     C                   call      'TIBS02R'
009900010601     C                   parm                    KPJBA
010000010601     C                   parm                    TIBS02DS
010100010601     C                   if        t02err =  *blanks
010200160718     C                   movel     t02ke1        v1cmau
010300160718     C                   movel     t02uni        v1cmaud
010400010601     C                   SETON                                        90
010500010601     C                   ELSE
010600010612     C                   SETON                                        28
010700010612     C                   MOVEL     MSG(1)        V1CMSG
010800010601     C                   GOTO      ENDSRCONTR
010900010601     C                   endif
011000010604     C                   ENDIF
011100010601
011200010611      * controllo se campo valorizzato
011300160718     C                   IF        V1Cmau = *ZEROS OR V1Cmau = *BLANKS
011400160718     C                   MOVE      *BLANKS       V1cmaud
011500010612     C                   SETON                                        28        controllo P.O.emi.
011600010612     C                   MOVEL     MSG(1)        V1CMSG
011700010601     C                   GOTO      ENDSRCONTR                                   normale
011800010601     C                   ENDIF
011900010611
012000010611     C     ENDSRCONTR    endsr
012100010608      **********************************************************************
012200010608      * CONTROLLI VIDEO 2
012300010608      **********************************************************************
012400010608     C     Sr_Contr02    begsr
012500010608      * Reset indicatori
012600160718     C                   setoff                                       28
012700010608
012800010608     C     ENDSRCONT2    endsr
012900010608      **********************************************************************
013000010608      * CARICAMENTO SECONDA VIDEATA
013100010608      **********************************************************************
013200010608     C     Sr_Carvid     begsr
013300010608
013400160718     C                   movel     v1cmau        v2cmau
013500160718     C                   movel     *blanks       v2cmaud
013600160718     C                   movel     *blank        v2csefi
013700160718     C                   movel     v1cmau        kke1
013800160718     C                   movel     'MAU'         kcod
013900160715     C     ktntbe        chain     tntbe01l
014000160715     C                   if        %found(tntbe01l)
014100160718     C                   movel     tbeuni        dmau
014200160718     C                   movel     §maudes       v2cmaud
014300160718     C                   move      §mauuso       v2csefi
014400160715     c                   exsr      sr_contr02
014500010608     C                   endif
014600010608
014700010608     C                   endsr
014800010608      **********************************************************************
014900010608      * REGISTRAZIONE DATI IMMESSI
015000010608      **********************************************************************
015100010608     C     Sr_Registra   begsr
015200010608
015300160718     C                   movel     'MAU'         kcod
015400160718     C                   movel     v2cmau        kke1
015500160715     C     ktntbe        chain     tntbe01l
015600160718     C                   if        not %found(tntbe01l)
015700160718     C                   movel     'MAU'         tbecod
015800160718     C                   movel     v2cmau        tbeke1
015900060502     c                   clear                   tbeflt
016000010608     C                   move      *blanks       tbeftt
016100010608     C                   move      *blanks       tbeftr
016200010608     C                   move      datoggi       tbedtr
016300160718     C                   movel     v2cmaud       §maudes
016400160718     C                   movel     v2csefi       §mauuso
016500160718     C                   movel     dmau          tbeuni
016600010608     C                   write     tntbe000
016700010608     C                   else
016800060502     C                   clear                   tbeflt
016900010608     C                   move      *blanks       tbeftt
017000010608     C                   move      *blanks       tbeftr
017100010608     C                   move      datoggi       tbedtr
017200160718     C                   movel     v2cmaud       §maudes
017300160718     C                   movel     v2csefi       §mauuso
017400160718     C                   movel     dmau          tbeuni
017500010608     C                   update    tntbe000
017600010608     C                   endif
017700010608     C     ENDREGISTRA   endsr
017800980923      *****************************************************************
017900980923      * ROUTINE INIZIALE
018000980923      *****************************************************************
018100980923     C     *INZSR        BEGSR
018200980923      *
018300980521     C     *ENTRY        PLIST
018400980521     C                   PARM                    KPJBA
018500001030
018600980521     C                   Z-ADD     1             CODUT
018700980521     C                   CALL      'X§PARUT'
018800980521     C                   PARM                    UT§DSE0F
018900980521     C                   MOVEL     RAGUT         RSUT
019000980521     C                   MOVEL     REC80         CNCR80
019100001221
019200010608      * reperisco data e ora
019300010608     C                   TIME                    W0140
019400010608      * UDATE IN GGMMAAAA
019500010608     C                   MOVE      W0140         WDTGIO
019600010608      * UDATE IN AAAAMMGG
019700010608     C                   Z-ADD     WDTGIO        G02DAT
019800010608     C                   MOVEL     *BLANK        G02ERR
019900010608     C                   CALL      'XSRDA8'
020000010608     C                   PARM                    WLBDAT
020100010608     C                   MOVEL     G02INV        DATEU
020200010608     C                   MOVEL     DATEU         DAToggi
020300010608
020400010601     C     KTNTBE        KLIST
020500010604     C                   KFLD                    KCOD
020600010601     C                   KFLD                    KKE1
020700010611
020800001006     C                   endsr
020900980521      *---------------------------------------------------------------------------------------------
021000980521** MSG  Lungh. 78                                                            *
021100160718Inserire codice marchio
021200160718marchio non gestibile
021300010612Inserire tutti i dati
