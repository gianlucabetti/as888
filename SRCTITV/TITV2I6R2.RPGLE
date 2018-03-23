000100130315      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200130306      *
000300130306      * PARTICOLARITA':
000400130306      * barcode = prendere la numerazione di 18 CARATTERI dal byte 41 x 18 bytes
000500130306      *
000600121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000700000313     F*
000800990910     Ftivin00r  uF   E             DISK    usropn
000900130315     FEDIVATwr  O    E             DISK    usropn
001000010330     D*----------------------------------------------------
001100010330     D* DICHIARAZIOINE VARIABILI DI WRK
001200010330     D*----------------------------------------------------
001300010330     D dscmz         e ds                  inz
001400010330     D psds           sds
001500010330     D  procname         *PROC
001600010330     D tivlrds       e ds                  extname(tivlr00f)
001700010330     D esito           s              1
001800010330     D prmlit          s             10
001900010330     D prmfir          s             10
002000010330     D wrkesito        s                   like(esito)
002100010330     D rrnum           s              6  0 INZ(*zeros)
002200130315     D wNomeFile       s             30    INZ(*blanks)
002300130315     D wCMR            s             35    INZ(*blanks)
002400130315     D wPosDaA         s              2    INZ(*blanks)
002500130315     D wPosDa          s              2  0 INZ(*zeros)
002600130315     D wLungA          s              2    INZ(*blanks)
002700130315     D wLung           s              2  0 INZ(*zeros)
002800130315     D wDataOraAlfa    s             14    INZ(*blanks)
002900010330     D*------------------
003000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003100010330     D*------------------
003200010330     D WLBDA8          DS                  INZ
003300010330     D  G08DAT                 1      8  0
003400010330     D  G08INV                 9     16  0
003500010330     D  G08ERR                17     17
003600010330     D  G08TGI                18     22  0
003700000313     D*
003800010330
003900010330
004000010330
004100000913     C                   reset                   rrnum
004200990921     C                   reset                   esito
004300990921     C                   reset                   wrkesito
004400010601     C*
004500010601     C                   exsr      opeini
004600040301     C                   exsr      rwvat
004700010601     C*
004800010601     C                   seton                                        lr
004900010601
005000010601
005100010601
005200010601
005300010601     C*--------------------------------------------------------
005400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005500010601     C*--------------------------------------------------------
005600010601     C     PREELA        BEGSR
005700010601     C*
005800010601     C* SE OCCORRE SPEDIRE IN FILIALE
005900010601     C                   if        invfil <> *zeros and
006000010601     C                             flgGiro = '0'
006100010601     C*
006200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006300010601     C                   eval      flgGiro = '1'
006400010601     C*
006500010601     C                   endif
006600010601     C*
006700010601     C                   ENDSR
006800010601     C***
006900010601
007000010601
007100010601
007200010601     C*--------------------------------------------------------
007300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007400010601     C*--------------------------------------------------------
007500010601     C     ENDELA        BEGSR
007600990915     C*
007700000616     C*
007800010601     C                   ENDSR
007900010601     C***
008000010601
008100010330     C*--------------------------------------------------------
008200130315     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVAT0F              *
008300010330     C*--------------------------------------------------------
008400040301     C     RWVAT         BEGSR
008500010330     C*
008600010330     C                   if        not %open(tivin00r)
008700010330     C                   open      tivin00r
008800010330     C                   endif
008900130315     C                   if        not %open(edivatwr)
009000130315     C                   open      edivatwr
009100010330     C                   endif
009200010330     C*
009300010604     C                   clear                   §CTROK
009400010604     C                   clear                   §CTRMO
009500010604     C                   clear                   §CTRNO
009600010330     C*
009700010330     C                   DO        *HIVAL
009800010330     C*
009900010330     C                   READ      tivin00r                               70
010000010618     C*
010100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010200010618     C                   if        vinflg = '1'
010300010618     C                   eval      flgOk = '1'
010400010618     C                   endif
010500010618     C*
010600010330     C                   if        vindta > *blanks
010700010330     C                   add       1             rrnum
010800010330     C*
010900010601     C                   if        *in70 = *off and
011000010330     C                             (vinflg = *blanks
011100010330     C                              or vinflg = '0'
011200010330     C                              or vinflg = '2')
011300010330     C*
011400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011500010711     C                   if        vinflg = *blanks or vinflg = '0'
011600010711     C                   clear                   vinmsg
011700010711     C                   endif
011800010601     C*
011900040301     C* Inizializzo campi d wrk e imposto i valori d default
012000010330     C                   exsr      inzvar
012100010330     C                   exsr      defcam
012200010601     C*
012300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012400040301     C                   eval      depfil = VATLNP
012500010601     C                   exsr      repfil
012600010601     C                   if        depfil = invfil
012700021025     C                   if        vlrpoi = 999
012800040301     C                   MOVE(P)   invfil        VATFGS
012900021025     C                   else
013000040301     C                   MOVE(P)   vlrpoi        VATFGS
013100021025     C                   endif
013200050114     C                   endif
013300040301     C*
013400130315     C* Imposto e scarico il buffer del EDIVAT
013500040301     C                   exsr      impvat
013600010601     C*
013700010601     C                   exsr      PREELA
013800010601     C*
013900010604     C* Ebbene...
014000010604     C*
014100010604     C  N31              ADD       1             §CTROK            7 0
014200010604     C   32              ADD       1             §CTRMO            7 0
014300010604     C   31              ADD       1             §CTRNO            7 0
014400010604     C*
014500020722     C                   endif
014600020722     C*
014700010604     C                   if        *in31 = *off and
014800010604     C                             *in32 = *off
014900010604     C                   eval      vinflg = '1'
015000010604     C                   else
015100010604     C                   eval      vinflg = '2'
015200010604     C                   endif
015300010604     C*
015400010330     C                   else
015500010330     C                   eval      vinflg = '1'
015600010330     C                   endif
015700010601     C*
015800010601     C  N70              update    tivin000
015900010330     C*
016000010330     C  N70              ENDdo
016100010601     C
016200010601     C*
016300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016400010601     C                   if        cntNonEl = *zeros or
016500010601     C                             flgMulti = '0'
016600010330     C* Se non ci sono record con errori ...
016700010601     C                   if        §ctrno = 0 and
016800010604     C                             §ctrmo = 0 and
016900010601     C                             flgStato <> '2'
017000010330     C* ... restituisco esito OK.
017100010330     C                   eval      wrkesito = '0'
017200010330     C                   else
017300010330     C                   if        §ctrok > 0
017400010330     C                   eval      wrkesito = '1'
017500010330     C                   else
017600010615     C                   if        flgOk = '0'
017700010615     C                   eval      wrkesito = '2'
017800010615     C                   else
017900010615     C                   eval      wrkesito = '6'
018000010615     C                   endif
018100010330     C                   endif
018200010330     C                   endif
018300010601     C                   else
018400010601     C                   eval      wrkesito = '9'
018500010601     C                   endif
018600010330     C*
018700010330     C                   if        %open(tivin00r)
018800010330     C                   close     tivin00r
018900010330     C                   endif
019000130315     C                   if        %open(edivatwr)
019100130315     C                   close     edivatwr
019200010330     C                   endif
019300010601     C*
019400010601     C                   if        vlrpoi <> 999
019500010601     C                   eval      invfil = vlrpoi
019600010601     C                   endif
019700010330     C*
019800010330     C                   if        §ctrok > 0
019900010601     C                             and invfil > *zeros
020000010330     C                   exsr      invio
020100010330     C                   endif
020200010601     C*
020300010618     C                   if        flgGiro = '1'
020400010601     C                   exsr      endela
020500010618     C                   endif
020600010330     C*
020700010330     C                   ENDSR
020800010330     C***
020900010330
021000010601
021100010601
021200010601
021300010601
021400010330     C*----------------------------------------------------*
021500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021600010330     C*----------------------------------------------------*
021700010330     C     INZVAR        BEGSR
021800051219     C*
021900010330     C*
022000010330     C                   ENDSR
022100010330     C*----------------------------------------------------*
022200020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022300010330     C*----------------------------------------------------*
022400010330     C     DEFCAM        BEGSR
022500010330     C*
022600020204     C* Inizializzo il buffer del record da scrivere
022700130315     C                   CLEAR                   EDIVAT00
022800020204     C* Imposto i valori di default...
022900130315     C                   EVAL      VATCCM = 2751348
023000130315     C                   EVAL      VATLNP = 275
023100040301     C                   EVAL      VATTRC = 'E'
023200020204     C* ... e poi verifico se sono stati passati come parametri
023300020204     C                   IF        vlrppt > *blanks
023400040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023600020204     C                   EXSR      CHKNUM
023700020204     C                   IF        PiInt=*on
023800040301     C                   Z-ADD     PiVal         VATCCM
023900020204     C                   ENDIF
024000040301     C                   ENDIF
024100040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024300020204     C                   EXSR      CHKNUM
024400020204     C                   IF        PiInt=*on
024500040301     C                   Z-ADD     PiVal         VATLNP
024600020204     C                   ENDIF
024700040301     C                   ENDIF
024800130315     C*
024900130315     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
025000130315     C                   move(p)   vlrMSG        wNomeFile
025100130315     C*
025200130315     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
025300130315     C* del codice CMR dal nome del file importato.
025400130315     C                   if        wNomeFile <> *blanks AND
025500130315     C                             vlrppt<>*blanks
025600130315     C                   eval      wPosDaA = %subst(vlrppt:19:2)
025700130315     C                   move(p)   wPosDaA       wPosDa
025800130315     C                   eval      wLungA  = %subst(vlrppt:21:2)
025900130315     C                   move(p)   wLungA        wLung
026000130315     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
026100130315     C                   else
026200130315     C                   movel     datcor        wDataOraAlfa
026300130315     C                   move      oracor        wDataOraAlfa
026400130315     C                   eval      wCMR = 'INVIO DEL: ' + wDataOraAlfa
026500130315     C                   endif
026600130315     C*
026700020204     C                   ENDIF
026800020204     C*
026900010330     C                   ENDSR
027000010607     C*----------------------------------------------------*
027100130315     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
027200010607     C*----------------------------------------------------*
027300040301     C     IMPVAT        BEGSR
027400010607     C*
027500010607     C                   SETOFF                                       3132
027600130315     C                   MOVEL     datcor        VATAAS
027700040316     C*
027800040316     C* Campi "calcolati"
027900121024     C*
028000010607     C*
028100010607     C* Reperimento campi ALFA
028200121024     C                   EVAL      VATNOT =
028300130315     C                                      %trim(%subst(vindta:13:8))
028400040301     C*
028500040301     C* Reperimento campi NUMERICI
028600130306     C                   EVAL      PiStr=%trim(%subst(vindta:3:8))
028700040301     C                   EXSR      CHKNUM
028800040301     C                   IF        PiInt=*on
028900040301     C                   Z-ADD     PiVal         VATNSP
029000040301     C                   ELSE
029100040301     C                   SETON                                        31
029200040301     C                   Z-ADD     *zeros        VATNSP
029300040301     C                   EVAL      vinmsg = %trimr(vinmsg)
029400040301     C                             + ' ' + 'VATNSP'
029500040301     C                   ENDIF
029600130315     C*
029700130315     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
029800130315     C
029900130315     C                   EVAL      VATCMR = wCMR
030000130315     C                   EVAL      VATCNT = 1
030100040301     C*
030200130315     C* Scarico il buffer del EDIVAT
030300130315     C  N31              WRITE     EDIVAT00
030400020204     C*
030500010607     C                   ENDSR
030600010607     C*----------------------------------------------------*
030700020204
030800010330
030900010330
031000010330     C*----------------------------------------------------*
031100010330     C*  CONTROLLO NUMERICITA' CAMPI
031200010330     C*----------------------------------------------------*
031300010330     C     CHKNUM        BEGSR
031400010330     C*
031500010606     C                   IF        PiDecChr = *blanks
031600010606     C                   EVAL      PiDecChr = ','
031700010606     C                   ENDIF
031800010606     C*
031900010606     C                   CALL(e)   'ISNUMERIC'
032000010330     C                   PARM                    PiStr            30
032100010606     C                   PARM                    PiDecChr          1
032200010330     C                   PARM      *ZEROS        PiVal            30 9
032300010330     C                   PARM      '0'           PiInt             1
032400010330     C                   PARM      '0'           PiNum             1
032500010330     C                   IF        %error
032600010606     C                   EVAL      PiNum=*off
032700010330     C                   ENDIF
032800010330     C*
032900010330     C                   ENDSR
033000010330     C***
033100010330
033200010601
033300010601
033400010601
033500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
033600010601     C     repfil        BEGSR
033700010601     C*
033800010601     C                   if        invfil = *zeros and
033900010601     C                             depfil > *zeros and
034000010629     C                             (vinflg = *blanks or
034100010629     C                              vinflg = *zeros)
034200010601     C
034300010601     C                   eval      invfil = depfil
034400010601     C                   endif
034500010601     C*
034600010601     C                   if        depfil <> invfil and
034700010601     C                             invfil > *zeros
034800010601     C                   eval      flgMulti = '1'
034900010601     C                   if        vinflg = *blanks
035000010601     C                   add       1             cntNonEl
035100010601     C                   endif
035200010601     C                   endif
035300010601     C*
035400010601     C                   if        vinflg = '2'
035500010601     C                   eval      flgStato = '2'
035600010601     C                   endif
035700010601     C*
035800010601     C                   ENDSR
035900010601     C***
036000010601
036100010601
036200010601
036300010330
036400010330
036500010330
036600990920      /TITLE Invio dei dati al punto operativo.
036700000613     C     invio         BEGSR
036800990920     C*
036900990920     C                   reset                   dscmz
037000010601     C                   move      invfil        cmzdst
037100990920     C                   eval      cmzfld = vlrfou
037200990920     C                   eval      cmzmbd = vlrhdl
037300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
037400000710     C                   if        prmfir = *blanks
037500130315     C                   eval      cmzfla = 'EDIVAT0F'
037600130315     C                   eval      cmzmba = 'EDIVAT0F'
037700000710     C                   else
037800000710     C                   eval      cmzfla = prmfir
037900000710     C                   eval      cmzmba = prmfir
038000000710     C                   endif
038100990920     C                   eval      cmznrr = *zeros
038200990920     C                   move      §ctrok        cmznrr
038300021018     C                   eval      cmzlba = vlrfl1
038400990920     C                   call(e)   'TIS711C'
038500990920     C                   parm                    dscmz
038600990921     C                   parm      *blanks       esito
038700990920     C                   if        %error
038800990920     C                             or cmzerr = '1'
038900990921     C                             or esito  = '1'
039000000710     C                   eval      wrkesito = '3'
039100990920     C                   endif
039200990920     C*
039300000613     C                   ENDSR
039400990910
039500010601
039600010601
039700010601
039800010601
039900010601      /TITLE Invio dei dati al punto operativo.
040000010601     C     opeini        BEGSR
040100010601     C*
040200010601     C* Inizializzo flag e contatori operativi
040300010601     C                   movel     '0'           flgGiro           1
040400010601     C                   movel     '0'           flgMulti          1
040500010601     C                   movel     '1'           flgStato          1
040600010615     C                   movel     '0'           flgOk             1
040700010601     C                   z-add     *zeros        cntNonEl         10 0
040800010601     C                   z-add     *zeros        depfil            3 0
040900010601     C                   z-add     *zeros        invfil            3 0
041000010601     C*
041100010601     C                   ENDSR
041200010601     C***
041300010601
041400010601
041500010601
041600010330
041700010330
041800000613     C     *inzsr        BEGSR
041900990910     C*
042000990910     C     *entry        plist
042100990920     C                   parm                    tivlrds
042200990921     C                   parm      wrkesito      esito
042300000724     C                   parm                    prmlit
042400000710     C                   parm                    prmfir
042500010330     C*
042600010330     C* CALCOLA LA DATA CORRENTE
042700010330     C                   time                    wn14             14 0
042800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
042900010330     C                   z-add     wn8           g08dat
043000010330     C                   z-add     *zeros        g08inv
043100010330     C                   movel     '0'           g08err
043200010330     C                   call      'XSRDA8'
043300010330     C                   parm                    wlbda8
043400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
043500130315     C                   time                    wn14             14 0
043600130315     C                   movel     wn14          oracor            6 0          *ORA
043700000613     C*
043800000613     C                   ENDSR
043900000613     C***
