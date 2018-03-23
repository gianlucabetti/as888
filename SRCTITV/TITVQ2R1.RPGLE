000100060112      /TITLE Upload via Internet: traduzione in EDIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500060112     FEDIVATwr  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400010330     D esito           s              1
001500010330     D prmlit          s             10
001600010330     D prmfir          s             10
001700010330     D wrkesito        s                   like(esito)
001800010330     D rrnum           s              6  0 INZ(*zeros)
001900010330     D*------------------
002000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002100010330     D*------------------
002200010330     D WLBDA8          DS                  INZ
002300010330     D  G08DAT                 1      8  0
002400010330     D  G08INV                 9     16  0
002500010330     D  G08ERR                17     17
002600010330     D  G08TGI                18     22  0
002700010612     D*
002800010612     D fnvataar        ds                  inz
002900060109     D  a_vatatb                      1
003000060109     D  a_vatccm                      7
003100060109     D  a_vatlnp                      3
003200060109     D  a_vataas                      4
003300060109     D  a_vatmgs                      4
003400060109     D  a_vatnrs                      2
003500060109     D  a_vatnsp                      7
003600070326     D  a_vatldv              18     28
003700060109     D  a_vattrc                      1
003800071122     D* a_vatnot                     35
003900071122     D  a_vatnot                     31
004000010330
004100010330
004200010330
004300000913     C                   reset                   rrnum
004400990921     C                   reset                   esito
004500990921     C                   reset                   wrkesito
004600010601     C*
004700010601     C                   exsr      opeini
004800010612     C                   exsr      rwvat
004900010601     C*
005000010601     C                   seton                                        lr
005100010601
005200010601
005300010601
005400010601
005500010601     C*--------------------------------------------------------
005600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005700010601     C*--------------------------------------------------------
005800010601     C     PREELA        BEGSR
005900010601     C*
006000010601     C* SE OCCORRE SPEDIRE IN FILIALE
006100010601     C                   if        invfil <> *zeros and
006200010601     C                             flgGiro = '0'
006300010601     C*
006400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006500010601     C                   eval      flgGiro = '1'
006600010601     C*
006700010601     C                   endif
006800010601     C*
006900010601     C                   ENDSR
007000010601     C***
007100010601
007200010601
007300010601
007400010601
007500010601
007600010601     C*--------------------------------------------------------
007700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007800010601     C*--------------------------------------------------------
007900010601     C     ENDELA        BEGSR
008000000616     C*
008100010601     C                   ENDSR
008200010601     C***
008300000613
008400010330
008500010601
008600010601
008700010601
008800010330     C*--------------------------------------------------------
008900060112     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
009000010330     C*--------------------------------------------------------
009100010612     C     RWVAT         BEGSR
009200010330     C*
009300010330     C                   if        not %open(tivin00r)
009400010330     C                   open      tivin00r
009500010330     C                   endif
009600060112     C                   if        not %open(edivatwr)
009700060112     C                   open      edivatwr
009800010330     C                   endif
009900010330     C*
010000010604     C                   clear                   §CTROK
010100010604     C                   clear                   §CTRMO
010200010604     C                   clear                   §CTRNO
010300010330     C*
010400010330     C                   DO        *HIVAL
010500010330     C*
010600010330     C                   READ      tivin00r                               70
010700010618     C*
010800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010900010618     C                   if        vinflg = '1'
011000010618     C                   eval      flgOk = '1'
011100010618     C                   endif
011200010618     C*
011300010330     C                   if        vindta > *blanks
011400010330     C                   add       1             rrnum
011500010330     C*
011600010601     C                   if        *in70 = *off and
011700010330     C                             (vinflg = *blanks
011800010330     C                              or vinflg = '0'
011900010330     C                              or vinflg = '2')
012000010330     C*
012100010330     C                   clear                   vinmsg
012200010613     C                   eval      fnvataar = vindta
012300010601     C*
012400010330     C                   exsr      inzvar
012500010330     C                   exsr      defcam
012600010612     C                   exsr      impvat
012700010601     C*
012800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012900010612     C                   eval      depfil = VATLNP
013000010601     C                   exsr      repfil
013100010601     C                   if        depfil = invfil
013200021014     C                   if        vlrpoi = 999
013300021010     C                   MOVE(P)   invfil        VATFGS
013400021014     C                   else
013500021014     C                   MOVE(P)   vlrpoi        VATFGS
013600021014     C                   endif
013700010601     C*
013800010601     C                   exsr      PREELA
013900060112     C*
014000060112     C* VALORIZZO CAMPI RELATIVI AL "CMR"
014100070417     C                   EVAL      VATCMR = %trim(a_vatldv)
014200060112     C                   EVAL      VATCNT = 1
014300010601     C*
014400010604     C* Ebbene...
014500010604     C*
014600010604     C  N31              ADD       1             §CTROK            7 0
014700010604     C   32              ADD       1             §CTRMO            7 0
014800010604     C   31              ADD       1             §CTRNO            7 0
014900060112     C  N31              WRITE     EDIVAT00
015000010604     C*
015100010604     C                   if        *in31 = *off and
015200010604     C                             *in32 = *off
015300010604     C                   eval      vinflg = '1'
015400010604     C                   else
015500010604     C                   eval      vinflg = '2'
015600010604     C                   endif
015700010604     C                   endif
015800010601     C*
015900010604     C                   endif
016000010604     C*
016100010330     C                   else
016200010330     C                   eval      vinflg = '1'
016300010330     C                   endif
016400010601     C*
016500010601     C  N70              update    tivin000
016600010330     C*
016700010330     C  N70              ENDdo
016800010601     C
016900010601     C*
017000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017100010601     C                   if        cntNonEl = *zeros or
017200010601     C                             flgMulti = '0'
017300010330     C* Se non ci sono record con errori ...
017400010601     C                   if        §ctrno = 0 and
017500010604     C                             §ctrmo = 0 and
017600010601     C                             flgStato <> '2'
017700010330     C* ... restituisco esito OK.
017800010330     C                   eval      wrkesito = '0'
017900010330     C                   else
018000010330     C                   if        §ctrok > 0
018100010330     C                   eval      wrkesito = '1'
018200010330     C                   else
018300010615     C                   if        flgOk = '0'
018400010615     C                   eval      wrkesito = '2'
018500010615     C                   else
018600010615     C                   eval      wrkesito = '6'
018700010615     C                   endif
018800010330     C                   endif
018900010330     C                   endif
019000010601     C                   else
019100010601     C                   eval      wrkesito = '9'
019200010601     C                   endif
019300010330     C*
019400010330     C                   if        %open(tivin00r)
019500010330     C                   close     tivin00r
019600010330     C                   endif
019700060112     C                   if        %open(edivatwr)
019800060112     C                   close     edivatwr
019900010330     C                   endif
020000010601     C*
020100010601     C                   if        vlrpoi <> 999
020200010601     C                   eval      invfil = vlrpoi
020300010601     C                   endif
020400010330     C*
020500010330     C                   if        §ctrok > 0
020600010601     C                             and invfil > *zeros
020700010330     C                   exsr      invio
020800010330     C                   endif
020900010601     C*
021000010612     C                   if        flgGiro = '1'
021100010601     C                   exsr      endela
021200010612     C                   endif
021300010330     C*
021400010330     C                   ENDSR
021500010330     C***
021600010330
021700010601
021800010601
021900010601
022000010601
022100010330     C*----------------------------------------------------*
022200010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022300010330     C*----------------------------------------------------*
022400010330     C     INZVAR        BEGSR
022500010330     C*
022600010330     C*
022700010330     C                   ENDSR
022800010330     C*----------------------------------------------------*
022900010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023000010330     C*----------------------------------------------------*
023100010330     C     DEFCAM        BEGSR
023200010330     C*
023300060112     C                   CLEAR                   EDIVAT00
023400010330     C*
023500010330     C                   ENDSR
023600010705     C*----------------------------------------------------*
023700010705
023800010705
023900010705
024000010705
024100010705     C*----------------------------------------------------*
024200060112     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
024300010705     C*----------------------------------------------------*
024400010705     C     IMPVAT        BEGSR
024500010705     C*
024600010705     C                   SETOFF                                       3132
024700010705     C*
024800010705     C* Reperimento campi ALFA
024900010705     C                   EVAL      VATATB=a_vatatb
025000010705     C                   EVAL      VATTRC=a_vattrc
025100020725     C                   EVAL      VATNOT=%trim(a_vatnot)
025200010705     C*
025300010705     C                   EVAL      PiStr=%trim(a_vatccm)
025400010705     C                   EXSR      CHKNUM
025500010705     C                   IF        PiInt=*on
025600010705     C                   Z-ADD     PiVal         VATCCM
025700010705     C                   ELSE
025800010705     C                   SETON                                        31
025900010705     C                   EVAL      vinmsg = %trimr(vinmsg)
026000010705     C                             + ' ' + 'VATCCM'
026100010705     C                   ENDIF
026200010705     C*
026300010705     C                   EVAL      PiStr=%trim(a_vatlnp)
026400010705     C                   EXSR      CHKNUM
026500010705     C                   IF        PiInt=*on
026600010705     C                   Z-ADD     PiVal         VATLNP
026700010705     C                   ELSE
026800010705     C                   SETON                                        31
026900010705     C                   EVAL      vinmsg = %trimr(vinmsg)
027000021010     C                             + ' ' + 'VATLNP'
027100010705     C                   ENDIF
027200010705     C*
027300060109     C                   EVAL      PiStr=%trim(a_vataas)
027400060109     C                   EXSR      CHKNUM
027500060109     C                   IF        PiInt=*on
027600060109     C                   Z-ADD     PiVal         VATAAS
027700060109     C                   ELSE
027800060109     C                   SETON                                        32
027900060109     C                   MOVEL     datcor        VATAAS
028000060109     C                   EVAL      vinmsg = %trimr(vinmsg)
028100060109     C                             + ' ' + 'VATAAS'
028200060109     C                   ENDIF
028300010705     C*
028400070215     C*                  EVAL      PiStr=%trim(a_vatnrs)
028500070215     C*                  IF        PiStr = *blanks
028600070215     C*                  EVAL      PiStr = *zeros
028700070215     C*                  ENDIF
028800070215     C*                  EXSR      CHKNUM
028900070215     C*                  IF        PiInt=*on
029000070215     C*                  Z-ADD     PiVal         VATNRS
029100070215     C*                  ELSE
029200070215     C*                  SETON                                        31
029300070215     C*                  EVAL      vinmsg = %trimr(vinmsg)
029400070215     C*                            + ' ' + 'VATNRS'
029500070215     C*                  ENDIF
029600010705     C*
029700010705     C                   EVAL      PiStr=%trim(a_vatnsp)
029800010705     C                   EXSR      CHKNUM
029900010705     C                   IF        PiInt=*on
030000010705     C                   Z-ADD     PiVal         VATNSP
030100010705     C                   ELSE
030200010705     C                   SETON                                        31
030300010705     C                   EVAL      vinmsg = %trimr(vinmsg)
030400010705     C                             + ' ' + 'VATNSP'
030500010705     C                   ENDIF
030600010705     C*
030700010705     C                   ENDSR
030800010705     C*----------------------------------------------------*
030900010705
031000010705
031100010705
031200010705
031300010705
031400010705     C*----------------------------------------------------*
031500010705     C*  CONTROLLO NUMERICITA' CAMPI
031600010705     C*----------------------------------------------------*
031700010705     C     CHKNUM        BEGSR
031800010705     C*
031900010705     C                   call(e)   'ISNUMERIC'
032000010705     C                   PARM                    PiStr            30
032100010705     C                   PARM      ','           PiDecChr          1
032200010705     C                   PARM      *ZEROS        PiVal            30 9
032300010705     C                   PARM      '0'           PiInt             1
032400010705     C                   PARM      '0'           PiNum             1
032500010705     C                   IF        %error
032600010705     C                   EVAL      PiInt=*off
032700010705     C                   ENDIF
032800010705     C*
032900010705     C                   ENDSR
033000010705     C*----------------------------------------------------*
033100010705
033200010705
033300010705
033400010601
033500010601
033600010601
033700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
033800010601     C     repfil        BEGSR
033900010601     C*
034000010601     C                   if        invfil = *zeros and
034100010601     C                             depfil > *zeros and
034200010613     C                             (vinflg = *blanks or
034300010613     C                              vinflg = *zeros)
034400010601     C
034500010601     C                   eval      invfil = depfil
034600010601     C                   endif
034700010601     C*
034800010601     C                   if        depfil <> invfil and
034900010601     C                             invfil > *zeros
035000010601     C                   eval      flgMulti = '1'
035100010601     C                   if        vinflg = *blanks
035200010601     C                   add       1             cntNonEl
035300010601     C                   endif
035400010601     C                   endif
035500010601     C*
035600010601     C                   if        vinflg = '2'
035700010601     C                   eval      flgStato = '2'
035800010601     C                   endif
035900010601     C*
036000010601     C                   ENDSR
036100010601     C***
036200010601
036300010601
036400010601
036500010330
036600010330
036700010330
036800990920      /TITLE Invio dei dati al punto operativo.
036900000613     C     invio         BEGSR
037000990920     C*
037100990920     C                   reset                   dscmz
037200010601     C                   move      invfil        cmzdst
037300990920     C                   eval      cmzfld = vlrfou
037400990920     C                   eval      cmzmbd = vlrhdl
037500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
037600000710     C                   if        prmfir = *blanks
037700060112     C                   eval      cmzfla = 'EDIVAT0F'
037800060112     C                   eval      cmzmba = 'EDIVAT0F'
037900000710     C                   else
038000000710     C                   eval      cmzfla = prmfir
038100000710     C                   eval      cmzmba = prmfir
038200000710     C                   endif
038300990920     C                   eval      cmznrr = *zeros
038400990920     C                   move      §ctrok        cmznrr
038500021003     C                   eval      cmzlba = vlrfl1
038600990920     C                   call(e)   'TIS711C'
038700990920     C                   parm                    dscmz
038800990921     C                   parm      *blanks       esito
038900990920     C                   if        %error
039000990920     C                             or cmzerr = '1'
039100990921     C                             or esito  = '1'
039200000710     C                   eval      wrkesito = '3'
039300990920     C                   endif
039400990920     C*
039500000613     C                   ENDSR
039600990910
039700010601
039800010601
039900010601
040000010601
040100010601      /TITLE Invio dei dati al punto operativo.
040200010601     C     opeini        BEGSR
040300010601     C*
040400010601     C* Inizializzo flag e contatori operativi
040500010601     C                   movel     '0'           flgGiro           1
040600010601     C                   movel     '0'           flgMulti          1
040700010601     C                   movel     '1'           flgStato          1
040800010615     C                   movel     '0'           flgOk             1
040900010601     C                   z-add     *zeros        cntNonEl         10 0
041000010601     C                   z-add     *zeros        depfil            3 0
041100010601     C                   z-add     *zeros        invfil            3 0
041200030407     C*
041300030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
041400030407     C                   if        %subst(vlrppt:1:1) = 'S'
041500030407     C                   seton                                        50
041600030407     C                   else
041700030407     C                   setoff                                       50
041800030407     C                   endif
041900010601     C*
042000010601     C                   ENDSR
042100010601     C***
042200010601
042300010601
042400010601
042500010330
042600010330
042700000613     C     *inzsr        BEGSR
042800990910     C*
042900990910     C     *entry        plist
043000990920     C                   parm                    tivlrds
043100990921     C                   parm      wrkesito      esito
043200000724     C                   parm                    prmlit
043300000710     C                   parm                    prmfir
043400010330     C*
043500010330     C* CALCOLA LA DATA CORRENTE
043600010330     C                   time                    wn14             14 0
043700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
043800010330     C                   z-add     wn8           g08dat
043900010330     C                   z-add     *zeros        g08inv
044000010330     C                   movel     '0'           g08err
044100010330     C                   call      'XSRDA8'
044200010330     C                   parm                    wlbda8
044300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
044400000613     C*
044500000613     C                   ENDSR
044600000613     C***
