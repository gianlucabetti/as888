000100030416      /TITLE Upload via Internet: traduzione FLBLCAAR in FIVABWWR.
000200130115     H dftactgrp(*no) actgrp(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020913     FFIVABwwr  O    E             DISK    usropn
000600990908
000700000512     D*------------
000800990920     D dscmz         e ds                  inz
000900990910     D psds           sds
001000990910     D  procname         *PROC
001100990920     D tivlrds       e ds                  extname(tivlr00f)
001200030416     D flblcaaf      e ds                  inz
001300990910     D esito           s              1
001400000724     D prmlit          s             10
001500000710     D prmfir          s             10
001600990921     D wrkesito        s                   like(esito)
001700000613     D rrnum           s              6  0 INZ(*zeros)
001800010606     D*------------------
001900010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
002000010606     D*------------------
002100010606     D WLBDA8          DS                  INZ
002200010606     D  G08DAT                 1      8  0
002300010606     D  G08INV                 9     16  0
002400010606     D  G08ERR                17     17
002500010606     D  G08TGI                18     22  0
002600010606     D*
002700990908
002800000913     C                   reset                   rrnum
002900990921     C                   reset                   esito
003000990921     C                   reset                   wrkesito
003100010530     C*
003200010530     C                   exsr      opeini
003300030416     C                   exsr      rwblc
003400010530     C*
003500010530     C                   seton                                        lr
003600010530
003700010530
003800010530
003900010530
004000010530     C*--------------------------------------------------------
004100010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004200010530     C*--------------------------------------------------------
004300010530     C     PREELA        BEGSR
004400000724     C*
004500000724     C* SE OCCORRE SPEDIRE IN FILIALE
004600010530     C                   if        invfil <> *zeros and
004700010530     C                             flgGiro = '0'
004800010530     C*
004900010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005000010530     C                   eval      flgGiro = '1'
005100010530     C*
005200010530     C                   endif
005300010530     C*
005400010530     C                   ENDSR
005500010530     C***
005600010530
005700010530
005800010530
005900010530
006000010530
006100010530
006200010530     C*--------------------------------------------------------
006300010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006400010530     C*--------------------------------------------------------
006500010530     C     ENDELA        BEGSR
006600010530     C*
006700010530     C                   ENDSR
006800010530     C***
006900030416
007000030416
007100030416
007200030416     C*----------------------------------------------------*
007300030416     C*  IMPOSTAZIONE CAMPI COSTANTI
007400030416     C*----------------------------------------------------*
007500030416     C     DEFCAM        BEGSR
007600030416     C*
007700030416     C* Verifico se sono stati passati come parametri
007800030416     C                   IF        vlrppt > *blanks
007900030416     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
008000030416     C                   EXSR      CHKNUM
008100030416     C                   IF        PiInt=*on
008200030416     C                   Z-ADD     PiVal         VABCCM
008300030416     C                   ENDIF
008400030416     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
008500030416     C                   EXSR      CHKNUM
008600030416     C                   IF        PiInt=*on
008700030416     C                   Z-ADD     PiVal         VABLNP
008800030416     C                   ENDIF
008900030416     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
009000030416     C                   EXSR      CHKNUM
009100030416     C                   IF        PiInt=*on
009200030416     C                   Z-ADD     PiVal         VABCTR
009300030416     C                   ENDIF
009400030416     C                   ENDIF
009500030416     C*
009600030416     C                   ENDSR
009700030416     C*----------------------------------------------------*
009800010530
009900010530
010000010530
010100910830     C*--------------------------------------------------------
010200030416     C* RWBLC   LEGGE tivin00r E DA flblcaaf SCRIVE fivabwwr *
010300910830     C*--------------------------------------------------------
010400030416     C     RWBLC         BEGSR
010500990910     C*
010600990914     C                   if        not %open(tivin00r)
010700990908     C                   open      tivin00r
010800990914     C                   endif
010900020913     C                   if        not %open(fivabwwr)
011000020913     C                   open      fivabwwr
011100990914     C                   endif
011200990910     C*
011300990910     C                   clear                   §CTROK
011400990910     C                   clear                   §CTRMO
011500990910     C                   clear                   §CTRNO
011600990910     C*
011700921023     C                   DO        *HIVAL
011800990913     C*
011900990915     C                   READ      tivin00r                               70
012000010618     C*
012100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012200010618     C                   if        vinflg = '1'
012300010618     C                   eval      flgOk = '1'
012400010618     C                   endif
012500010705     C*
012600010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
012700010705     C                   if        %subst(vindta:1:1) = '='
012800010705     C                   eval      %subst(vindta:1:1) = *blanks
012900010705     C                   endif
013000010705     C*
013100000905     C                   if        vindta > *blanks
013200000613     C                   add       1             rrnum
013300010601     C* ...elimino eventuali segni
013400010601     C     '+':' '       xlate     vindta        vindta
013500990913     C*
013600010601     C                   if        *in70 = *off and
013700990913     C                             (vinflg = *blanks
013800010711     C                              or vinflg = '0'
013900010711     C                              or vinflg = '2')
014000990913     C*
014100020913     C                   clear                   FIVAB000
014200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014300010711     C                   if        vinflg = *blanks or vinflg = '0'
014400010711     C                   clear                   vinmsg
014500010711     C                   endif
014600010711     C*
014700030416     C                   eval      flblcaaf = vindta
014800030416     C*
014900971021     C                   SETOFF                                       303132
015000030416     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
015100030416     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI OPPURE TUTTI BLANKS
015200030416     C                   TESTN                   ALCLNP               30
015300030416     C  N30              TESTN                   ALCLNP                 30
015400030416     C                   MOVE      ALCLNP        W001A             1
015500030416     C     *IN30         IFEQ      *OFF
015600030416     C     W001A         ORLT      '0'
015700030416     C                   SETON                                        31
015800030416     C                   EVAL      vinmsg = %trimr(vinmsg)
015900030416     C                             + ' ' + 'ALCLNP'
016000030416     C                   ELSE
016100030416     C                   MOVE      ALCLNP        VABLNP
016200030416     C     VABLNP        IFLE      *ZEROS
016300030416     C                   SETON                                        31
016400030416     C                   EVAL      vinmsg = %trimr(vinmsg)
016500030416     C                             + ' ' + 'ALCLNP'
016600030416     C                   ENDIF
016700030416     C                   ENDIF
016800030416     C*
016900030416     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017000030416     C                   eval      depfil = VABLNP
017100030416     C                   exsr      repfil
017200030416     C                   if        depfil = invfil
017300030416     C                   if        vlrpoi = 999
017400030416     C                   MOVE(P)   invfil        VABFGS
017500030416     C                   else
017600030416     C                   MOVE(P)   vlrpoi        VABFGS
017700030416     C                   endif
017800030416     C*
017900030416     C                   exsr      PREELA
018000030416     C*
018100030416     C                   TESTN                   ALCCCM               30
018200030416     C  N30              TESTN                   ALCCCM                 30
018300030416     C                   MOVE      ALCCCM        W001A             1
018400030416     C     *IN30         IFEQ      *OFF
018500030416     C     W001A         ORLT      '0'
018600030416     C                   SETON                                        31
018700030416     C                   EVAL      vinmsg = %trimr(vinmsg)
018800030416     C                             + ' ' + 'ALCCCM'
018900030416     C                   ELSE
019000030416     C                   MOVE      ALCCCM        VABCCM
019100030416     C     VABCCM        IFLE      *ZEROS
019200030416     C                   SETON                                        31
019300030416     C                   EVAL      vinmsg = %trimr(vinmsg)
019400030416     C                             + ' ' + 'ALCCCM'
019500030416     C                   ENDIF
019600030416     C                   ENDIF
019700040318     C*
019800040319     C                   MOVEL     datcor        VABAAS
019900030416     C*
020000030416     C                   TESTN                   ALCRMN               30
020100030416     C  N30              TESTN                   ALCRMN                 30
020200030416     C  N30              TESTN                   ALCRMN                   30
020300030416     C                   MOVE      ALCRMN        W001A             1
020400030416     C     *IN30         IFEQ      *OFF
020500030416     C     W001A         ORLT      '0'
020600030416     C                   MOVE      *ZERO         VABRMN
020700030416     C                   SETON                                        31
020800030416     C                   EVAL      vinmsg = %trimr(vinmsg)
020900030416     C                             + ' ' + 'ALCRMN'
021000030416     C                   ELSE
021100030416     C                   MOVE      ALCRMN        VABRMN
021200030416     C                   ENDIF
021300030416     C* SE ERRORI SCARTO
021400030416     C  N31              DO
021500030416     C                   MOVE      ALCATB        VABATB
021600030416     C                   MOVE      ALCMGS        VABMGS
021700030416     C                   MOVE      ALCNRS        VABNRS
021800030416     C                   MOVE      ALCNSP        VABNSP
021900030416     C                   MOVEL     ALCCBL        VABCBO
022000030416     C                   MOVE      ALCLNA        VABLNA
022100030416     C                   MOVEL     ALCRSD        VABRSD
022200030416     C                   MOVEL     ALCIND        VABIND
022300030416     C                   MOVE      ALCCAD        N5                5 0
022400030416     C                   MOVEL     N5            VABCAD
022500030416     C                   MOVEL     ALCLOD        VABLOD
022600030416     C                   MOVEL     ALCPRD        VABPRD
022700030416     C                   MOVEL     ALCGC1        VABGC1
022800030416     C                   MOVEL     ALCGC2        VABGC2
022900030416     C                   MOVE      ALCCTR        VABCTR
023000030416     C                   MOVE      ALCCTS        VABCTS
023100030416     C                   MOVE      ALCFTM        VABFTM
023200030416     C                   MOVE      ALCTSP        VABTSP
023300030416     C                   MOVE      ALCVAE        NUM9              9 0
023400030416     C                   Z-ADD     NUM9          VABIAS
023500030416     C                   MOVE      ALCNAS        VABNAS
023600030416     C                   MOVE      ALCNCL        VABNCL
023700030416     C* IMPOSTO LA VIRGOLA NEL PESO
023800030416     C                   MOVE      ALCPKG        §PKGD             1 0
023900030416     C                   MOVEL     ALCPKG        §PKGI             7 0
024000030416     C                   MOVE      §PKGI         §PKG6             6
024100030416     C                   MOVE      §PKGD         VABPKB
024200030416     C                   MOVEL     §PKG6         VABPKB
024300041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
024400041001     C                   IF        *IN51 = *OFF
024500030416     C* IMPOSTO LA VIRGOLA NEL VOLUME
024600030416     C                   MOVE      ALCVOL        §VOLD             2 0
024700030416     C                   MOVEL     ALCVOL        §VOLI             4 0
024800030416     C                   MOVE      §VOLI         §VOL3             3
024900030416     C                   MOVE      §VOLD         §VOL              5 2
025000030416     C                   MOVEL     §VOL3         §VOL
025100030416     C                   Z-ADD     §VOL          VABVLB
025200041001     C                   ELSE
025300041001     C* ...altrimenti forzo azzeramento del volume
025400041001     C                   Z-ADD     *zeros        VABVLB
025500041001     C                   ENDIF
025600030416     C*
025700030416     C                   MOVE      ALCANT        VABANT
025800030416     C                   MOVE      ALCASS        NUM9              9 0
025900030416     C                   Z-ADD     NUM9          VABCAS
026000030416     C                   MOVE      ALCTIC        VABTIC
026100030416     C                   MOVE      ALCCEI        VABTC1
026200030416     C                   MOVE      ALCSCL        VABSCL
026300030416     C                   MOVEL     ALCRAM        VABRMA
026400030416     C                   MOVEL     ALCRMO        VABRMO
026500030416     C                   MOVE      ALCNCD        VABNCD
026600030416     C                   MOVE      ALCNCA        VABNCA
026700030416     C                   MOVE      ALCXCO        VABXCO
026800030416     C                   MOVE      ALCNOT        VABNOT
026900030416     C                   MOVE      ALCZNC        VABZNC
027000030416     C                   MOVE      ALCCTM        VABCTM
027100030416     C                   MOVE      ALCFFD        VABFFD
027200030416     C                   MOVE      ALCDCR        VABDCR
027300030416     C                   MOVE      ALCTCR        VABTCR
027400030416     C                   MOVE      ALCHCR        VABHCR
027500030416     C                   ENDDO
027600030416     C*
027700030416     C* AL TERMINE IMPOSTO I VALORI PASSATI NEI PARAMETRI DEL TRADUTTORE
027800030416     C                   EXSR      DEFCAM
027900930409     C*
028000010604     C  N31              ADD       1             §CTROK            7 0
028100010604     C   32              ADD       1             §CTRMO            7 0
028200010604     C   31              ADD       1             §CTRNO            7 0
028300030416     C  N31              WRITE     FIVAB000
028400990910     C*
028500010601     C                   if        *in31 = *off and
028600010601     C                             *in32 = *off
028700990910     C                   eval      vinflg = '1'
028800990910     C                   else
028900990910     C                   eval      vinflg = '2'
029000990910     C                   endif
029100990910     C                   endif
029200010601     C*
029300010601     C                   endif
029400000905     C*
029500000905     C                   else
029600000905     C                   eval      vinflg = '1'
029700000905     C                   endif
029800000905     C*
029900000905     C  N70              update    tivin000
030000991022     C*
030100991022     C  N70              ENDdo
030200010530     C
030300010530     C*
030400010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030500010530     C                   if        cntNonEl = *zeros or
030600010530     C                             flgMulti = '0'
030700990910     C* Se non ci sono record con errori ...
030800010530     C                   if        §ctrno = 0 and
030900010601     C                             §ctrmo = 0 and
031000010530     C                             flgStato <> '2'
031100990910     C* ... restituisco esito OK.
031200990921     C                   eval      wrkesito = '0'
031300990910     C                   else
031400010601     C                   if        §ctrok > 0
031500990921     C                   eval      wrkesito = '1'
031600000710     C                   else
031700010615     C                   if        flgOk = '0'
031800010615     C                   eval      wrkesito = '2'
031900010615     C                   else
032000010615     C                   eval      wrkesito = '6'
032100010615     C                   endif
032200990910     C                   endif
032300000710     C                   endif
032400010530     C                   else
032500010530     C                   eval      wrkesito = '9'
032600010530     C                   endif
032700990910     C*
032800990914     C                   if        %open(tivin00r)
032900990908     C                   close     tivin00r
033000990914     C                   endif
033100020913     C                   if        %open(fivabwwr)
033200020913     C                   close     fivabwwr
033300990914     C                   endif
033400010530     C*
033500010530     C                   if        vlrpoi <> 999
033600010530     C                   eval      invfil = vlrpoi
033700010530     C                   endif
033800990910     C*
033900990920     C                   if        §ctrok > 0
034000010530     C                             and invfil > *zeros
034100000613     C                   exsr      invio
034200990920     C                   endif
034300010612     C*
034400010612     C                   if        flgGiro = '1'
034500010612     C                   exsr      endela
034600010612     C                   endif
034700990920     C*
034800910830     C                   ENDSR
034900030416     C*----------------------------------------------------*
035000030416
035100030416
035200030416
035300030416
035400030416     C*----------------------------------------------------*
035500030416     C*  CONTROLLO NUMERICITA' CAMPI
035600030416     C*----------------------------------------------------*
035700030416     C     CHKNUM        BEGSR
035800030416     C*
035900030416     C                   call(e)   'ISNUMERIC'
036000030416     C                   PARM                    PiStr            30
036100030416     C                   PARM      ','           PiDecChr          1
036200030416     C                   PARM      *ZEROS        PiVal            30 9
036300030416     C                   PARM      '0'           PiInt             1
036400030416     C                   PARM      '0'           PiNum             1
036500030416     C                   IF        %error
036600030416     C                   EVAL      PiInt=*off
036700030416     C                   EVAL      PiNum=*off
036800030416     C                   ENDIF
036900030416     C*
037000030416     C                   ENDSR
037100030416     C*----------------------------------------------------*
037200990920
037300010530
037400010530
037500010530
037600010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
037700010530     C     repfil        BEGSR
037800010530     C*
037900010530     C                   if        invfil = *zeros and
038000010530     C                             depfil > *zeros and
038100010613     C                             (vinflg = *blanks or
038200010613     C                              vinflg = *zeros)
038300010530     C
038400010530     C                   eval      invfil = depfil
038500010530     C                   endif
038600010530     C*
038700010530     C                   if        depfil <> invfil and
038800010530     C                             invfil > *zeros
038900010530     C                   eval      flgMulti = '1'
039000010530     C                   if        vinflg = *blanks
039100010530     C                   add       1             cntNonEl
039200010530     C                   endif
039300010530     C                   endif
039400010530     C*
039500010530     C                   if        vinflg = '2'
039600010530     C                   eval      flgStato = '2'
039700010530     C                   endif
039800010530     C*
039900010530     C                   ENDSR
040000010530     C***
040100010530
040200010530
040300010530
040400010530
040500990920      /TITLE Invio dei dati al punto operativo.
040600000613     C     invio         BEGSR
040700990920     C*
040800990920     C                   reset                   dscmz
040900010530     C                   move      invfil        cmzdst
041000990920     C                   eval      cmzfld = vlrfou
041100990920     C                   eval      cmzmbd = vlrhdl
041200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
041300000710     C                   if        prmfir = *blanks
041400020913     C                   eval      cmzfla = 'FIVAB00F'
041500020913     C                   eval      cmzmba = 'FIVAB00F'
041600000710     C                   else
041700000710     C                   eval      cmzfla = prmfir
041800000710     C                   eval      cmzmba = prmfir
041900000710     C                   endif
042000990920     C                   eval      cmznrr = *zeros
042100990920     C                   move      §ctrok        cmznrr
042200021003     C                   eval      cmzlba = vlrfl1
042300990920     C                   call(e)   'TIS711C'
042400990920     C                   parm                    dscmz
042500990921     C                   parm      *blanks       esito
042600990920     C                   if        %error
042700990920     C                             or cmzerr = '1'
042800990921     C                             or esito  = '1'
042900000710     C                   eval      wrkesito = '3'
043000990920     C                   endif
043100990920     C*
043200000613     C                   ENDSR
043300000613     C***
043400010530
043500010530
043600010530
043700010530
043800010530      /TITLE Invio dei dati al punto operativo.
043900010530     C     opeini        BEGSR
044000010530     C*
044100010530     C* Inizializzo flag e contatori operativi
044200010530     C                   movel     '0'           flgGiro           1
044300010530     C                   movel     '0'           flgMulti          1
044400010530     C                   movel     '1'           flgStato          1
044500010615     C                   movel     '0'           flgOk             1
044600010530     C                   z-add     *zeros        cntNonEl         10 0
044700010530     C                   z-add     *zeros        depfil            3 0
044800010530     C                   z-add     *zeros        invfil            3 0
044900030407     C*
045000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
045100030416     C                   if        %subst(vlrppt:14:1) = 'S'
045200030407     C                   seton                                        50
045300030407     C                   else
045400030407     C                   setoff                                       50
045500030407     C                   endif
045600041001     C*
045700041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
045800041001     C                   if        %subst(vlrppt:15:1) = 'S'
045900041001     C                   seton                                        51
046000041001     C                   else
046100041001     C                   setoff                                       51
046200041001     C                   endif
046300010530     C*
046400010530     C                   ENDSR
046500010530     C***
046600010530
046700010530
046800010530
046900990910
047000000613     C     *inzsr        BEGSR
047100990910     C*
047200990910     C     *entry        plist
047300990920     C                   parm                    tivlrds
047400990921     C                   parm      wrkesito      esito
047500000724     C                   parm                    prmlit
047600000710     C                   parm                    prmfir
047700010606     C*
047800010606     C* CALCOLA LA DATA CORRENTE
047900010606     C                   time                    wn14             14 0
048000010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
048100010606     C                   z-add     wn8           g08dat
048200010606     C                   z-add     *zeros        g08inv
048300010606     C                   movel     '0'           g08err
048400010606     C                   call      'XSRDA8'
048500010606     C                   parm                    wlbda8
048600010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
048700000613     C*
048800000613     C                   ENDSR
048900000613     C***
