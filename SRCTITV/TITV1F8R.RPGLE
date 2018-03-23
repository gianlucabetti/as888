000100080410      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200171006     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040301     FFIVATwwr  O    E             DISK    usropn
000600000313     D*
000700000613     D*------------
000800000613     D* COMANDI
000900000613     D*------------
001000011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D dscmz         e ds                  inz
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800010330     D esito           s              1
001900010330     D prmlit          s             10
002000010330     D prmfir          s             10
002100010330     D wrkesito        s                   like(esito)
002200010330     D wrkdata         s               d
002300010330     D wrkora          s               t
002400010330     D rrnum           s              6  0 INZ(*zeros)
002500011113     D depcmd          s            150    INZ(*blanks)
002600010330     D*------------------
002700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800010330     D*------------------
002900010330     D WLBDA8          DS                  INZ
003000010330     D  G08DAT                 1      8  0
003100010330     D  G08INV                 9     16  0
003200010330     D  G08ERR                17     17
003300010330     D  G08TGI                18     22  0
003400000313     D*
003500010330
003600010330
003700010330
003800990915     C                   time                    wrkdata
003900990915     C                   time                    wrkora
004000000913     C                   reset                   rrnum
004100990921     C                   reset                   esito
004200990921     C                   reset                   wrkesito
004300010601     C*
004400010601     C                   exsr      opeini
004500040301     C                   exsr      rwvat
004600010601     C*
004700010601     C                   seton                                        lr
004800010601
004900010601
005000010601
005100010601
005200010601     C*--------------------------------------------------------
005300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005400010601     C*--------------------------------------------------------
005500010601     C     PREELA        BEGSR
005600010601     C*
005700010601     C* SE OCCORRE SPEDIRE IN FILIALE
005800010601     C                   if        invfil <> *zeros and
005900010601     C                             flgGiro = '0'
006000010601     C*
006100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006200010601     C                   eval      flgGiro = '1'
006300010601     C*
006400010601     C                   endif
006500010601     C*
006600010601     C                   ENDSR
006700010601     C***
006800010601
006900010601
007000010601
007100010601     C*--------------------------------------------------------
007200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007300010601     C*--------------------------------------------------------
007400010601     C     ENDELA        BEGSR
007500000616     C*
007600010601     C                   ENDSR
007700010601     C***
007800010601
007900010330     C*--------------------------------------------------------
008000040301     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
008100010330     C*--------------------------------------------------------
008200040301     C     RWVAT         BEGSR
008300010330     C*
008400010330     C                   if        not %open(tivin00r)
008500010330     C                   open      tivin00r
008600010330     C                   endif
008700040301     C                   if        not %open(fivatwwr)
008800040301     C                   open      fivatwwr
008900010330     C                   endif
009000010330     C*
009100010604     C                   clear                   低TROK
009200010604     C                   clear                   低TRMO
009300010604     C                   clear                   低TRNO
009400010330     C*
009500010330     C                   DO        *HIVAL
009600010330     C*
009700010330     C                   READ      tivin00r                               70
009800010618     C*
009900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010000010618     C                   if        vinflg = '1'
010100010618     C                   eval      flgOk = '1'
010200010618     C                   endif
010300010618     C*
010400010330     C                   if        vindta > *blanks
010500010330     C                   add       1             rrnum
010600010330     C*
010700010601     C                   if        *in70 = *off and
010800010330     C                             (vinflg = *blanks
010900010330     C                              or vinflg = '0'
011000010330     C                              or vinflg = '2')
011100010330     C*
011200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011300010711     C                   if        vinflg = *blanks or vinflg = '0'
011400010711     C                   clear                   vinmsg
011500010711     C                   endif
011600010601     C*
011700040301     C* Inizializzo campi d wrk e imposto i valori d default
011800010330     C                   exsr      inzvar
011900010330     C                   exsr      defcam
012000010601     C*
012100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012200040301     C                   eval      depfil = VATLNP
012300010601     C                   exsr      repfil
012400010601     C                   if        depfil = invfil
012500021025     C                   if        vlrpoi = 999
012600040301     C                   MOVE(P)   invfil        VATFGS
012700021025     C                   else
012800040301     C                   MOVE(P)   vlrpoi        VATFGS
012900021025     C                   endif
013000050114     C                   endif
013100040301     C*
013200040301     C* Imposto e scarico il buffer del FIVAT
013300040301     C                   exsr      impvat
013400010601     C*
013500010601     C                   exsr      PREELA
013600010601     C*
013700010604     C* Ebbene...
013800010604     C*
013900010604     C  N31              ADD       1             低TROK            7 0
014000010604     C   32              ADD       1             低TRMO            7 0
014100010604     C   31              ADD       1             低TRNO            7 0
014200010604     C*
014300020722     C                   endif
014400020722     C*
014500010604     C                   if        *in31 = *off and
014600010604     C                             *in32 = *off
014700010604     C                   eval      vinflg = '1'
014800010604     C                   else
014900010604     C                   eval      vinflg = '2'
015000010604     C                   endif
015100010604     C*
015200010330     C                   else
015300010330     C                   eval      vinflg = '1'
015400010330     C                   endif
015500010601     C*
015600010601     C  N70              update    tivin000
015700010330     C*
015800010330     C  N70              ENDdo
015900010601     C
016000010601     C*
016100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016200010601     C                   if        cntNonEl = *zeros or
016300010601     C                             flgMulti = '0'
016400010330     C* Se non ci sono record con errori ...
016500010601     C                   if        呃trno = 0 and
016600010604     C                             呃trmo = 0 and
016700010601     C                             flgStato <> '2'
016800010330     C* ... restituisco esito OK.
016900010330     C                   eval      wrkesito = '0'
017000010330     C                   else
017100010330     C                   if        呃trok > 0
017200010330     C                   eval      wrkesito = '1'
017300010330     C                   else
017400010615     C                   if        flgOk = '0'
017500010615     C                   eval      wrkesito = '2'
017600010615     C                   else
017700010615     C                   eval      wrkesito = '6'
017800010615     C                   endif
017900010330     C                   endif
018000010330     C                   endif
018100010601     C                   else
018200010601     C                   eval      wrkesito = '9'
018300010601     C                   endif
018400010330     C*
018500010330     C                   if        %open(tivin00r)
018600010330     C                   close     tivin00r
018700010330     C                   endif
018800040301     C                   if        %open(fivatwwr)
018900040301     C                   close     fivatwwr
019000010330     C                   endif
019100010601     C*
019200010601     C                   if        vlrpoi <> 999
019300010601     C                   eval      invfil = vlrpoi
019400010601     C                   endif
019500010330     C*
019600010330     C                   if        呃trok > 0
019700010601     C                             and invfil > *zeros
019800010330     C                   exsr      invio
019900010330     C                   endif
020000010601     C*
020100010618     C                   if        flgGiro = '1'
020200010601     C                   exsr      endela
020300010618     C                   endif
020400010330     C*
020500010330     C                   ENDSR
020600010330     C***
020700010330
020800010601
020900010601
021000010601
021100010601
021200010330     C*----------------------------------------------------*
021300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021400010330     C*----------------------------------------------------*
021500010330     C     INZVAR        BEGSR
021600051219     C*
021700010330     C*
021800010330     C                   ENDSR
021900010330     C*----------------------------------------------------*
022000020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022100010330     C*----------------------------------------------------*
022200010330     C     DEFCAM        BEGSR
022300010330     C*
022400020204     C* Inizializzo il buffer del record da scrivere
022500040301     C                   CLEAR                   FIVAT000
022600020204     C* Imposto i valori di default...
022700080415     C                   EVAL      VATCCM = 1310416
022800080415     C                   EVAL      VATLNP = 275
022900040301     C                   EVAL      VATTRC = 'E'
023000020204     C* ... e poi verifico se sono stati passati come parametri
023100020204     C                   IF        vlrppt > *blanks
023200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023400020204     C                   EXSR      CHKNUM
023500020204     C                   IF        PiInt=*on
023600040301     C                   Z-ADD     PiVal         VATCCM
023700020204     C                   ENDIF
023800040301     C                   ENDIF
023900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024100020204     C                   EXSR      CHKNUM
024200020204     C                   IF        PiInt=*on
024300040301     C                   Z-ADD     PiVal         VATLNP
024400020204     C                   ENDIF
024500040301     C                   ENDIF
024600020204     C                   ENDIF
024700020204     C*
024800010330     C                   ENDSR
024900010607     C*----------------------------------------------------*
025000040301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
025100010607     C*----------------------------------------------------*
025200040301     C     IMPVAT        BEGSR
025300010607     C*
025400010607     C                   SETOFF                                       3132
025500040316     C*
025600040316     C* Campi "calcolati"
025700040316     C                   MOVEL     datcor        VATAAS
025800010607     C*
025900010607     C* Reperimento campi ALFA
026000080415     C                   EVAL      VATNOT = %trim(%subst(vindta:31:13))
026100040301     C*
026200040301     C* Reperimento campi NUMERICI
026300080415     C                   EVAL      PiStr=%trim(%subst(vindta:24:6))
026400040301     C                   EXSR      CHKNUM
026500040301     C                   IF        PiInt=*on
026600040301     C                   Z-ADD     PiVal         VATNSP
026700040301     C                   ELSE
026800040301     C                   SETON                                        31
026900040301     C                   Z-ADD     *zeros        VATNSP
027000040301     C                   EVAL      vinmsg = %trimr(vinmsg)
027100040301     C                             + ' ' + 'VATNSP'
027200040301     C                   ENDIF
027300040301     C*
027400040301     C* Scarico il buffer del FIVAT
027500040301     C  N31              WRITE     FIVAT000
027600020204     C*
027700010607     C                   ENDSR
027800010607     C*----------------------------------------------------*
027900020204
028000010330
028100010330
028200010330     C*----------------------------------------------------*
028300010330     C*  CONTROLLO NUMERICITA' CAMPI
028400010330     C*----------------------------------------------------*
028500010330     C     CHKNUM        BEGSR
028600010330     C*
028700010606     C                   IF        PiDecChr = *blanks
028800010606     C                   EVAL      PiDecChr = ','
028900010606     C                   ENDIF
029000010606     C*
029100010606     C                   CALL(e)   'ISNUMERIC'
029200010330     C                   PARM                    PiStr            30
029300010606     C                   PARM                    PiDecChr          1
029400010330     C                   PARM      *ZEROS        PiVal            30 9
029500010330     C                   PARM      '0'           PiInt             1
029600010330     C                   PARM      '0'           PiNum             1
029700010330     C                   IF        %error
029800010606     C                   EVAL      PiNum=*off
029900010330     C                   ENDIF
030000010330     C*
030100010330     C                   ENDSR
030200010330     C***
030300010330
030400010601
030500010601
030600010601
030700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
030800010601     C     repfil        BEGSR
030900010601     C*
031000010601     C                   if        invfil = *zeros and
031100010601     C                             depfil > *zeros and
031200010629     C                             (vinflg = *blanks or
031300010629     C                              vinflg = *zeros)
031400010601     C
031500010601     C                   eval      invfil = depfil
031600010601     C                   endif
031700010601     C*
031800010601     C                   if        depfil <> invfil and
031900010601     C                             invfil > *zeros
032000010601     C                   eval      flgMulti = '1'
032100010601     C                   if        vinflg = *blanks
032200010601     C                   add       1             cntNonEl
032300010601     C                   endif
032400010601     C                   endif
032500010601     C*
032600010601     C                   if        vinflg = '2'
032700010601     C                   eval      flgStato = '2'
032800010601     C                   endif
032900010601     C*
033000010601     C                   ENDSR
033100010601     C***
033200010601
033300010601
033400010601
033500010330
033600010330
033700010330
033800990920      /TITLE Invio dei dati al punto operativo.
033900000613     C     invio         BEGSR
034000990920     C*
034100990920     C                   reset                   dscmz
034200010601     C                   move      invfil        cmzdst
034300990920     C                   eval      cmzfld = vlrfou
034400990920     C                   eval      cmzmbd = vlrhdl
034500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
034600000710     C                   if        prmfir = *blanks
034700040301     C                   eval      cmzfla = 'FIVAT00F'
034800040301     C                   eval      cmzmba = 'FIVAT00F'
034900000710     C                   else
035000000710     C                   eval      cmzfla = prmfir
035100000710     C                   eval      cmzmba = prmfir
035200000710     C                   endif
035300990920     C                   eval      cmznrr = *zeros
035400990920     C                   move      呃trok        cmznrr
035500021018     C                   eval      cmzlba = vlrfl1
035600990920     C                   call(e)   'TIS711C'
035700990920     C                   parm                    dscmz
035800990921     C                   parm      *blanks       esito
035900990920     C                   if        %error
036000990920     C                             or cmzerr = '1'
036100990921     C                             or esito  = '1'
036200000710     C                   eval      wrkesito = '3'
036300990920     C                   endif
036400990920     C*
036500000613     C                   ENDSR
036600990910
036700010601
036800010601
036900010601
037000010601
037100010601      /TITLE Invio dei dati al punto operativo.
037200010601     C     opeini        BEGSR
037300010601     C*
037400010601     C* Inizializzo flag e contatori operativi
037500010601     C                   movel     '0'           flgGiro           1
037600010601     C                   movel     '0'           flgMulti          1
037700010601     C                   movel     '1'           flgStato          1
037800010615     C                   movel     '0'           flgOk             1
037900010601     C                   z-add     *zeros        cntNonEl         10 0
038000010601     C                   z-add     *zeros        depfil            3 0
038100010601     C                   z-add     *zeros        invfil            3 0
038200010601     C*
038300010601     C                   ENDSR
038400010601     C***
038500010601
038600010601
038700010601
038800010330
038900010330
039000000613     C     *inzsr        BEGSR
039100990910     C*
039200990910     C     *entry        plist
039300990920     C                   parm                    tivlrds
039400990921     C                   parm      wrkesito      esito
039500000724     C                   parm                    prmlit
039600000710     C                   parm                    prmfir
039700010330     C*
039800010330     C* CALCOLA LA DATA CORRENTE
039900010330     C                   time                    wn14             14 0
040000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
040100010330     C                   z-add     wn8           g08dat
040200010330     C                   z-add     *zeros        g08inv
040300010330     C                   movel     '0'           g08err
040400010330     C                   call      'XSRDA8'
040500010330     C                   parm                    wlbda8
040600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
040700000613     C*
040800000613     C                   ENDSR
040900000613     C***
