000100070517      /TITLE Upload via Internet: traduzione FLBLCAAR in EDIVABWR.
000200130115     H dftactgrp(*no) actgrp(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500070517     FEDIVABwr  O    E             DISK    usropn
000600990908
000700000512     D*------------
000800990920     D dscmz         e ds                  inz
000900990910     D psds           sds
001000990910     D  procname         *PROC
001100990920     D tivlrds       e ds                  extname(tivlr00f)
001200030416     D flblcaaf      e ds                  inz
001300070517     D tisi95ds      e ds
001400990910     D esito           s              1
001500000724     D prmlit          s             10
001600000710     D prmfir          s             10
001700990921     D wrkesito        s                   like(esito)
001800000613     D rrnum           s              6  0 INZ(*zeros)
001900070517     D wNomeFile       s             30    INZ(*blanks)
002000070517     D wCMR            s             35    INZ(*blanks)
002100070517     D wPosDaA         s              2    INZ(*blanks)
002200070517     D wPosDa          s              2  0 INZ(*zeros)
002300070517     D wLungA          s              2    INZ(*blanks)
002400070517     D wLung           s              2  0 INZ(*zeros)
002500070517     D wDataOraAlfa    s             14    INZ(*blanks)
002600010606     D*------------------
002700010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800010606     D*------------------
002900010606     D WLBDA8          DS                  INZ
003000010606     D  G08DAT                 1      8  0
003100010606     D  G08INV                 9     16  0
003200010606     D  G08ERR                17     17
003300010606     D  G08TGI                18     22  0
003400010606     D*
003500990908
003600000913     C                   reset                   rrnum
003700990921     C                   reset                   esito
003800990921     C                   reset                   wrkesito
003900010530     C*
004000010530     C                   exsr      opeini
004100030416     C                   exsr      rwblc
004200070517     C*
004300070517     C* Esegue lancio TISI95R solo x chiusura
004400070517     C                   CLEAR                   TISI95DS
004500070517     C                   EVAL      I95TLA = 'C'
004600070517     C                   CALL      'TISI95R'
004700070517     C                   PARM                    TISI95DS
004800010530     C*
004900010530     C                   seton                                        lr
005000010530
005100010530
005200010530
005300010530
005400010530     C*--------------------------------------------------------
005500010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005600010530     C*--------------------------------------------------------
005700010530     C     PREELA        BEGSR
005800000724     C*
005900000724     C* SE OCCORRE SPEDIRE IN FILIALE
006000010530     C                   if        invfil <> *zeros and
006100010530     C                             flgGiro = '0'
006200010530     C*
006300010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006400010530     C                   eval      flgGiro = '1'
006500010530     C*
006600010530     C                   endif
006700010530     C*
006800010530     C                   ENDSR
006900010530     C***
007000010530
007100010530
007200010530
007300010530
007400010530
007500010530
007600010530     C*--------------------------------------------------------
007700010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007800010530     C*--------------------------------------------------------
007900010530     C     ENDELA        BEGSR
008000010530     C*
008100010530     C                   ENDSR
008200010530     C***
008300030416
008400030416
008500030416
008600030416     C*----------------------------------------------------*
008700030416     C*  IMPOSTAZIONE CAMPI COSTANTI
008800030416     C*----------------------------------------------------*
008900030416     C     DEFCAM        BEGSR
009000030416     C*
009100030416     C* Verifico se sono stati passati come parametri
009200030416     C                   IF        vlrppt > *blanks
009300070517     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:7))
009400030416     C                   EXSR      CHKNUM
009500030416     C                   IF        PiInt=*on
009600030416     C                   Z-ADD     PiVal         VABCCM
009700030416     C                   ENDIF
009800070517     C                   EVAL      PiStr=%trim(%subst(vlrppt:15:3))
009900030416     C                   EXSR      CHKNUM
010000030416     C                   IF        PiInt=*on
010100030416     C                   Z-ADD     PiVal         VABLNP
010200030416     C                   ENDIF
010300070517     C                   EVAL      PiStr=%trim(%subst(vlrppt:18:3))
010400030416     C                   EXSR      CHKNUM
010500030416     C                   IF        PiInt=*on
010600030416     C                   Z-ADD     PiVal         VABCTR
010700030416     C                   ENDIF
010800030416     C                   ENDIF
010900030416     C*
011000030416     C                   ENDSR
011100030416     C*----------------------------------------------------*
011200010530
011300010530
011400010530
011500910830     C*--------------------------------------------------------
011600070517     C* RWBLC   LEGGE tivin00r E DA flblcaaf SCRIVE EDIVABWR *
011700910830     C*--------------------------------------------------------
011800030416     C     RWBLC         BEGSR
011900070517     C*
012000070517     C                   clear                   §CTROK
012100070517     C                   clear                   §CTRMO
012200070517     C                   clear                   §CTRNO
012300070517     C*
012400070517     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
012500070517     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
012600070517     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
012700070517     C                   if        *in60 = *on
012800990914     C                   if        not %open(tivin00r)
012900990908     C                   open      tivin00r
013000990914     C                   endif
013100070517     C                   if        not %open(edivabwr)
013200070517     C                   open      edivabwr
013300990914     C                   endif
013400990910     C*
013500921023     C                   DO        *HIVAL
013600990913     C*
013700990915     C                   READ      tivin00r                               70
013800010618     C*
013900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014000010618     C                   if        vinflg = '1'
014100010618     C                   eval      flgOk = '1'
014200010618     C                   endif
014300010705     C*
014400010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
014500010705     C                   if        %subst(vindta:1:1) = '='
014600010705     C                   eval      %subst(vindta:1:1) = *blanks
014700010705     C                   endif
014800010705     C*
014900000905     C                   if        vindta > *blanks
015000000613     C                   add       1             rrnum
015100010601     C* ...elimino eventuali segni
015200010601     C     '+':' '       xlate     vindta        vindta
015300990913     C*
015400010601     C                   if        *in70 = *off and
015500990913     C                             (vinflg = *blanks
015600010711     C                              or vinflg = '0'
015700010711     C                              or vinflg = '2')
015800990913     C*
015900070517     C                   clear                   EDIVAB00
016000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016100010711     C                   if        vinflg = *blanks or vinflg = '0'
016200010711     C                   clear                   vinmsg
016300010711     C                   endif
016400010711     C*
016500030416     C                   eval      flblcaaf = vindta
016600030416     C*
016700971021     C                   SETOFF                                       303132
016800030416     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
016900030416     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI OPPURE TUTTI BLANKS
017000030416     C                   TESTN                   ALCLNP               30
017100030416     C  N30              TESTN                   ALCLNP                 30
017200030416     C                   MOVE      ALCLNP        W001A             1
017300030416     C     *IN30         IFEQ      *OFF
017400030416     C     W001A         ORLT      '0'
017500030416     C                   SETON                                        31
017600030416     C                   EVAL      vinmsg = %trimr(vinmsg)
017700030416     C                             + ' ' + 'ALCLNP'
017800030416     C                   ELSE
017900030416     C                   MOVE      ALCLNP        VABLNP
018000030416     C     VABLNP        IFLE      *ZEROS
018100030416     C                   SETON                                        31
018200030416     C                   EVAL      vinmsg = %trimr(vinmsg)
018300030416     C                             + ' ' + 'ALCLNP'
018400030416     C                   ENDIF
018500030416     C                   ENDIF
018600030416     C*
018700030416     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018800030416     C                   eval      depfil = VABLNP
018900030416     C                   exsr      repfil
019000030416     C                   if        depfil = invfil
019100030416     C                   if        vlrpoi = 999
019200030416     C                   MOVE(P)   invfil        VABFGS
019300030416     C                   else
019400030416     C                   MOVE(P)   vlrpoi        VABFGS
019500030416     C                   endif
019600030416     C*
019700030416     C                   exsr      PREELA
019800030416     C*
019900030416     C                   TESTN                   ALCCCM               30
020000030416     C  N30              TESTN                   ALCCCM                 30
020100030416     C                   MOVE      ALCCCM        W001A             1
020200030416     C     *IN30         IFEQ      *OFF
020300030416     C     W001A         ORLT      '0'
020400030416     C                   SETON                                        31
020500030416     C                   EVAL      vinmsg = %trimr(vinmsg)
020600030416     C                             + ' ' + 'ALCCCM'
020700030416     C                   ELSE
020800030416     C                   MOVE      ALCCCM        VABCCM
020900030416     C     VABCCM        IFLE      *ZEROS
021000030416     C                   SETON                                        31
021100030416     C                   EVAL      vinmsg = %trimr(vinmsg)
021200030416     C                             + ' ' + 'ALCCCM'
021300030416     C                   ENDIF
021400030416     C                   ENDIF
021500040318     C*
021600040319     C                   MOVEL     datcor        VABAAS
021700030416     C*
021800030416     C                   TESTN                   ALCRMN               30
021900030416     C  N30              TESTN                   ALCRMN                 30
022000030416     C  N30              TESTN                   ALCRMN                   30
022100030416     C                   MOVE      ALCRMN        W001A             1
022200030416     C     *IN30         IFEQ      *OFF
022300030416     C     W001A         ORLT      '0'
022400030416     C                   MOVE      *ZERO         VABRMN
022500030416     C                   SETON                                        31
022600030416     C                   EVAL      vinmsg = %trimr(vinmsg)
022700030416     C                             + ' ' + 'ALCRMN'
022800030416     C                   ELSE
022900030416     C                   MOVE      ALCRMN        VABRMN
023000030416     C                   ENDIF
023100030416     C* SE ERRORI SCARTO
023200030416     C  N31              DO
023300030416     C                   MOVE      ALCATB        VABATB
023400030416     C                   MOVE      ALCMGS        VABMGS
023500030416     C                   MOVE      ALCNRS        VABNRS
023600030416     C                   MOVE      ALCNSP        VABNSP
023700030416     C                   MOVEL     ALCCBL        VABCBO
023800030416     C                   MOVE      ALCLNA        VABLNA
023900030416     C                   MOVEL     ALCRSD        VABRSD
024000030416     C                   MOVEL     ALCIND        VABIND
024100030416     C                   MOVE      ALCCAD        N5                5 0
024200030416     C                   MOVEL     N5            VABCAD
024300030416     C                   MOVEL     ALCLOD        VABLOD
024400030416     C                   MOVEL     ALCPRD        VABPRD
024500030416     C                   MOVEL     ALCGC1        VABGC1
024600030416     C                   MOVEL     ALCGC2        VABGC2
024700030416     C                   MOVE      ALCCTR        VABCTR
024800030416     C                   MOVE      ALCCTS        VABCTS
024900030416     C                   MOVE      ALCFTM        VABFTM
025000030416     C                   MOVE      ALCTSP        VABTSP
025100030416     C                   MOVE      ALCVAE        NUM9              9 0
025200030416     C                   Z-ADD     NUM9          VABIAS
025300030416     C                   MOVE      ALCNAS        VABNAS
025400030416     C                   MOVE      ALCNCL        VABNCL
025500030416     C* IMPOSTO LA VIRGOLA NEL PESO
025600030416     C                   MOVE      ALCPKG        §PKGD             1 0
025700030416     C                   MOVEL     ALCPKG        §PKGI             7 0
025800030416     C                   MOVE      §PKGI         §PKG6             6
025900030416     C                   MOVE      §PKGD         VABPKB
026000030416     C                   MOVEL     §PKG6         VABPKB
026100041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
026200041001     C                   IF        *IN51 = *OFF
026300030416     C* IMPOSTO LA VIRGOLA NEL VOLUME
026400030416     C                   MOVE      ALCVOL        §VOLD             2 0
026500030416     C                   MOVEL     ALCVOL        §VOLI             4 0
026600030416     C                   MOVE      §VOLI         §VOL3             3
026700030416     C                   MOVE      §VOLD         §VOL              5 2
026800030416     C                   MOVEL     §VOL3         §VOL
026900030416     C                   Z-ADD     §VOL          VABVLB
027000041001     C                   ELSE
027100041001     C* ...altrimenti forzo azzeramento del volume
027200041001     C                   Z-ADD     *zeros        VABVLB
027300041001     C                   ENDIF
027400030416     C*
027500030416     C                   MOVE      ALCANT        VABANT
027600030416     C                   MOVE      ALCASS        NUM9              9 0
027700030416     C                   Z-ADD     NUM9          VABCAS
027800030416     C                   MOVE      ALCTIC        VABTIC
027900030416     C                   MOVE      ALCCEI        VABTC1
028000030416     C                   MOVE      ALCSCL        VABSCL
028100030416     C                   MOVEL     ALCRAM        VABRMA
028200030416     C                   MOVEL     ALCRMO        VABRMO
028300030416     C                   MOVE      ALCNCD        VABNCD
028400030416     C                   MOVE      ALCNCA        VABNCA
028500030416     C                   MOVE      ALCXCO        VABXCO
028600030416     C                   MOVE      ALCNOT        VABNOT
028700030416     C                   MOVE      ALCZNC        VABZNC
028800030416     C                   MOVE      ALCCTM        VABCTM
028900030416     C                   MOVE      ALCFFD        VABFFD
029000030416     C                   MOVE      ALCDCR        VABDCR
029100030416     C                   MOVE      ALCTCR        VABTCR
029200030416     C                   MOVE      ALCHCR        VABHCR
029300030416     C                   ENDDO
029400030416     C*
029500030416     C* AL TERMINE IMPOSTO I VALORI PASSATI NEI PARAMETRI DEL TRADUTTORE
029600030416     C                   EXSR      DEFCAM
029700070517     C*
029800070517     C* VALORIZZO CAMPI RELATIVI AL "CMR"
029900070517     C                   EVAL      VABCMR = wCMR
030000070517     C                   EVAL      VABDCM = DATCOR
030100070517     C                   EVAL      VABDTS = DATCOR
030200070517     C                   EVAL      VABHMS = ORACOR
030300070517     C                   EVAL      VABCNT = 1
030400070517     C*
030500070517     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
030600070517     C                   IF        *IN52 = *ON
030700070517     C* Reperisco la provincia dal CAP e dalla località
030800070517     C                   IF        VABCAD <> *blanks AND
030900070517     C                             VABPRD  = *blanks AND
031000070517     C                             VABNZD  = *blanks
031100070517     C                   CLEAR                   TISI95DS
031200070517     C                   EVAL      I95TCN = '3'
031300070517     C                   Z-ADD     datcor        I95DAT
031400070517     C                   EVAL      I95CAP = VABCAD
031500070517     C                   EVAL      I95LOC = VABLOD
031600070517     C                   CALL      'TISI95R'
031700070517     C                   PARM                    TISI95DS
031800070517     C                   EVAL      VABPRD = O95PRV
031900070517     C                   ENDIF
032000070517     C                   ENDIF
032100930409     C*
032200010604     C  N31              ADD       1             §CTROK            7 0
032300010604     C   32              ADD       1             §CTRMO            7 0
032400010604     C   31              ADD       1             §CTRNO            7 0
032500070517     C  N31              WRITE     EDIVAB00
032600990910     C*
032700010601     C                   if        *in31 = *off and
032800010601     C                             *in32 = *off
032900990910     C                   eval      vinflg = '1'
033000990910     C                   else
033100990910     C                   eval      vinflg = '2'
033200990910     C                   endif
033300990910     C                   endif
033400010601     C*
033500010601     C                   endif
033600000905     C*
033700000905     C                   else
033800000905     C                   eval      vinflg = '1'
033900000905     C                   endif
034000000905     C*
034100000905     C  N70              update    tivin000
034200991022     C*
034300991022     C  N70              ENDdo
034400010530     C
034500010530     C*
034600010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034700010530     C                   if        cntNonEl = *zeros or
034800010530     C                             flgMulti = '0'
034900990910     C* Se non ci sono record con errori ...
035000010530     C                   if        §ctrno = 0 and
035100010601     C                             §ctrmo = 0 and
035200010530     C                             flgStato <> '2'
035300990910     C* ... restituisco esito OK.
035400990921     C                   eval      wrkesito = '0'
035500990910     C                   else
035600010601     C                   if        §ctrok > 0
035700990921     C                   eval      wrkesito = '1'
035800000710     C                   else
035900010615     C                   if        flgOk = '0'
036000010615     C                   eval      wrkesito = '2'
036100010615     C                   else
036200010615     C                   eval      wrkesito = '6'
036300010615     C                   endif
036400990910     C                   endif
036500000710     C                   endif
036600010530     C                   else
036700010530     C                   eval      wrkesito = '9'
036800010530     C                   endif
036900070517     C*
037000070517 X  1C                   else
037100070517     C                   eval      wrkesito = '8'
037200070517     C                   eval      flgGiro = '1'
037300070517 E  1C                   endif
037400990910     C*
037500990914     C                   if        %open(tivin00r)
037600990908     C                   close     tivin00r
037700990914     C                   endif
037800070517     C                   if        %open(edivabwr)
037900070517     C                   close     edivabwr
038000990914     C                   endif
038100010530     C*
038200010530     C                   if        vlrpoi <> 999
038300010530     C                   eval      invfil = vlrpoi
038400010530     C                   endif
038500990910     C*
038600990920     C                   if        §ctrok > 0
038700010530     C                             and invfil > *zeros
038800000613     C                   exsr      invio
038900990920     C                   endif
039000010612     C*
039100010612     C                   if        flgGiro = '1'
039200010612     C                   exsr      endela
039300010612     C                   endif
039400990920     C*
039500910830     C                   ENDSR
039600030416     C*----------------------------------------------------*
039700030416
039800030416
039900030416
040000030416
040100030416     C*----------------------------------------------------*
040200030416     C*  CONTROLLO NUMERICITA' CAMPI
040300030416     C*----------------------------------------------------*
040400030416     C     CHKNUM        BEGSR
040500030416     C*
040600030416     C                   call(e)   'ISNUMERIC'
040700030416     C                   PARM                    PiStr            30
040800030416     C                   PARM      ','           PiDecChr          1
040900030416     C                   PARM      *ZEROS        PiVal            30 9
041000030416     C                   PARM      '0'           PiInt             1
041100030416     C                   PARM      '0'           PiNum             1
041200030416     C                   IF        %error
041300030416     C                   EVAL      PiInt=*off
041400030416     C                   EVAL      PiNum=*off
041500030416     C                   ENDIF
041600030416     C*
041700030416     C                   ENDSR
041800030416     C*----------------------------------------------------*
041900990920
042000010530
042100010530
042200010530
042300010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
042400010530     C     repfil        BEGSR
042500010530     C*
042600010530     C                   if        invfil = *zeros and
042700010530     C                             depfil > *zeros and
042800010613     C                             (vinflg = *blanks or
042900010613     C                              vinflg = *zeros)
043000010530     C
043100010530     C                   eval      invfil = depfil
043200010530     C                   endif
043300010530     C*
043400010530     C                   if        depfil <> invfil and
043500010530     C                             invfil > *zeros
043600010530     C                   eval      flgMulti = '1'
043700010530     C                   if        vinflg = *blanks
043800010530     C                   add       1             cntNonEl
043900010530     C                   endif
044000010530     C                   endif
044100010530     C*
044200010530     C                   if        vinflg = '2'
044300010530     C                   eval      flgStato = '2'
044400010530     C                   endif
044500010530     C*
044600010530     C                   ENDSR
044700010530     C***
044800010530
044900010530
045000010530
045100010530
045200990920      /TITLE Invio dei dati al punto operativo.
045300000613     C     invio         BEGSR
045400990920     C*
045500990920     C                   reset                   dscmz
045600010530     C                   move      invfil        cmzdst
045700990920     C                   eval      cmzfld = vlrfou
045800990920     C                   eval      cmzmbd = vlrhdl
045900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
046000000710     C                   if        prmfir = *blanks
046100070517     C                   eval      cmzfla = 'EDIVAB0F'
046200070517     C                   eval      cmzmba = 'EDIVAB0F'
046300000710     C                   else
046400000710     C                   eval      cmzfla = prmfir
046500000710     C                   eval      cmzmba = prmfir
046600000710     C                   endif
046700990920     C                   eval      cmznrr = *zeros
046800990920     C                   move      §ctrok        cmznrr
046900021003     C                   eval      cmzlba = vlrfl1
047000990920     C                   call(e)   'TIS711C'
047100990920     C                   parm                    dscmz
047200990921     C                   parm      *blanks       esito
047300990920     C                   if        %error
047400990920     C                             or cmzerr = '1'
047500990921     C                             or esito  = '1'
047600000710     C                   eval      wrkesito = '3'
047700990920     C                   endif
047800990920     C*
047900000613     C                   ENDSR
048000000613     C***
048100010530
048200010530
048300010530
048400010530
048500010530      /TITLE Invio dei dati al punto operativo.
048600010530     C     opeini        BEGSR
048700010530     C*
048800010530     C* Inizializzo flag e contatori operativi
048900010530     C                   movel     '0'           flgGiro           1
049000010530     C                   movel     '0'           flgMulti          1
049100010530     C                   movel     '1'           flgStato          1
049200010615     C                   movel     '0'           flgOk             1
049300010530     C                   z-add     *zeros        cntNonEl         10 0
049400010530     C                   z-add     *zeros        depfil            3 0
049500010530     C                   z-add     *zeros        invfil            3 0
049600010530     C*
049700010530     C                   ENDSR
049800010530     C***
049900010530
050000010530
050100010530
050200990910
050300000613     C     *inzsr        BEGSR
050400990910     C*
050500990910     C     *entry        plist
050600990920     C                   parm                    tivlrds
050700990921     C                   parm      wrkesito      esito
050800000724     C                   parm                    prmlit
050900000710     C                   parm                    prmfir
051000070517     C*
051100070517     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
051200070517     C                   move(p)   vlrMSG        wNomeFile
051300070517     C*
051400070517     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
051500070517     C* del codice CMR dal nome del file importato.
051600070517     C                   if        wNomeFile <> *blanks AND
051700070517     C                             vlrppt<>*blanks
051800070517     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
051900070517     C                   if        %subst(vlrppt:1:1) = 'S'
052000070517     C                   seton                                        50
052100070517     C                   else
052200070517     C                   setoff                                       50
052300070517     C                   endif
052400070517     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
052500070517     C                   if        %subst(vlrppt:6:1) = 'S'
052600070517     C                   seton                                        51
052700070517     C                   else
052800070517     C                   setoff                                       51
052900070517     C                   endif
053000070517     C*
053100070517     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
053200070517     C* (naturalmente solo se nn già presente)
053300070517     C                   if        %subst(vlrppt:7:1) = 'S'
053400070517     C                   seton                                        52
053500070517     C                   else
053600070517     C                   setoff                                       52
053700070517     C                   endif
053800070517     C*
053900070517     C                   eval      wPosDaA = %subst(vlrppt:2:2)
054000070517     C                   move(p)   wPosDaA       wPosDa
054100070517     C                   eval      wLungA  = %subst(vlrppt:4:2)
054200070517     C                   move(p)   wLungA        wLung
054300070517     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
054400070517     C                   seton                                        60
054500070517     C                   else
054600070517     C                   if        vlrisv = 'UL'
054700070517     C                   movel     datcor        wDataOraAlfa
054800070517     C                   move      oracor        wDataOraAlfa
054900070517     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
055000070517     C                   seton                                        60
055100070517     C                   else
055200070517     C                   setoff                                       60
055300070517     C                   endif
055400070517     C                   endif
055500010606     C*
055600010606     C* CALCOLA LA DATA CORRENTE
055700010606     C                   time                    wn14             14 0
055800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
055900070517     C                   movel     wn14          oracor            6 0          *ORA
056000010606     C                   z-add     wn8           g08dat
056100010606     C                   z-add     *zeros        g08inv
056200010606     C                   movel     '0'           g08err
056300010606     C                   call      'XSRDA8'
056400010606     C                   parm                    wlbda8
056500010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
056600000613     C*
056700000613     C                   ENDSR
056800000613     C***
