000100060720      /TITLE Upload via Internet: traduzione in FIVABWWR. (x STONEFLY Cod. 0593024)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021014     FFIVABwwr  O    E             DISK    usropn
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
001900080321     D wNumSped        s              6  0 INZ(*zeros)
002000010330     D*------------------
002100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002200010330     D*------------------
002300010330     D WLBDA8          DS                  INZ
002400010330     D  G08DAT                 1      8  0
002500010330     D  G08INV                 9     16  0
002600010330     D  G08ERR                17     17
002700010330     D  G08TGI                18     22  0
002800000313     D*
002900010330
003000010330
003100010330
003200000913     C                   reset                   rrnum
003300990921     C                   reset                   esito
003400990921     C                   reset                   wrkesito
003500010601     C*
003600010601     C                   exsr      opeini
003700010605     C                   exsr      rwvab
003800010601     C*
003900010601     C                   seton                                        lr
004000010601
004100010601
004200010601
004300010601
004400010601     C*--------------------------------------------------------
004500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004600010601     C*--------------------------------------------------------
004700010601     C     PREELA        BEGSR
004800010601     C*
004900010601     C* SE OCCORRE SPEDIRE IN FILIALE
005000010601     C                   if        invfil <> *zeros and
005100010601     C                             flgGiro = '0'
005200010601     C*
005300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005400010601     C                   eval      flgGiro = '1'
005500010601     C*
005600010601     C                   endif
005700010601     C*
005800010601     C                   ENDSR
005900010601     C***
006000010601
006100010601
006200010601
006300010601     C*--------------------------------------------------------
006400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006500010601     C*--------------------------------------------------------
006600010601     C     ENDELA        BEGSR
006700000616     C*
006800010601     C                   ENDSR
006900010601     C***
007000000613
007100010601
007200010601
007300010330     C*--------------------------------------------------------
007400040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
007500010330     C*--------------------------------------------------------
007600010605     C     RWVAB         BEGSR
007700010330     C*
007800010330     C                   if        not %open(tivin00r)
007900010330     C                   open      tivin00r
008000010330     C                   endif
008100021014     C                   if        not %open(fivabwwr)
008200021014     C                   open      fivabwwr
008300010330     C                   endif
008400010330     C*
008500010604     C                   clear                   低TROK
008600010604     C                   clear                   低TRMO
008700010604     C                   clear                   低TRNO
008800060720     C*
008900060720     C                   exsr      inzvar
009000060720     C                   exsr      defcam
009100010330     C*
009200010330     C                   DO        *HIVAL
009300010330     C*
009400010330     C                   READ      tivin00r                               70
009500010618     C*
009600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009700010618     C                   if        vinflg = '1'
009800010618     C                   eval      flgOk = '1'
009900010618     C                   endif
010000010618     C*
010100010330     C                   if        vindta > *blanks
010200010330     C                   add       1             rrnum
010300010330     C*
010400010601     C                   if        *in70 = *off and
010500010330     C                             (vinflg = *blanks
010600010330     C                              or vinflg = '0'
010700010330     C                              or vinflg = '2')
010800010330     C*
010900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011000010711     C                   if        vinflg = *blanks or vinflg = '0'
011100010711     C                   clear                   vinmsg
011200010711     C                   endif
011300010601     C*
011400010605     C                   exsr      impvab
011500010601     C*
011600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011700010605     C                   eval      depfil = VABLNP
011800010601     C                   exsr      repfil
011900010601     C                   if        depfil = invfil
012000021025     C                   if        vlrpoi = 999
012100021025     C                   MOVE(P)   invfil        VABFGS
012200021025     C                   else
012300021025     C                   MOVE(P)   vlrpoi        VABFGS
012400021025     C                   endif
012500010601     C*
012600010601     C                   exsr      PREELA
012700010601     C*
012800010604     C* Ebbene...
012900010604     C*
013000010604     C  N31              ADD       1             低TROK            7 0
013100010604     C   32              ADD       1             低TRMO            7 0
013200010604     C   31              ADD       1             低TRNO            7 0
013300020205     C*
013400010604     C*
013500020722     C                   endif
013600020722     C*
013700010604     C                   if        *in31 = *off and
013800010604     C                             *in32 = *off
013900010604     C                   eval      vinflg = '1'
014000010604     C                   else
014100010604     C                   eval      vinflg = '2'
014200010604     C                   endif
014300010604     C                   endif
014400010604     C*
014500010330     C                   else
014600010330     C                   eval      vinflg = '1'
014700010330     C                   endif
014800010601     C*
014900010601     C  N70              update    tivin000
015000010330     C*
015100010330     C  N70              ENDdo
015200080321     C*
015300080321     C* Scarico la bolla rimasta in canna
015400091210     C                   seton                                        30
015500080321     C                   exsr      WRIVAB
015600010601     C*
015700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015800010601     C                   if        cntNonEl = *zeros or
015900010601     C                             flgMulti = '0'
016000010330     C* Se non ci sono record con errori ...
016100010601     C                   if        呃trno = 0 and
016200010604     C                             呃trmo = 0 and
016300010601     C                             flgStato <> '2'
016400010330     C* ... restituisco esito OK.
016500010330     C                   eval      wrkesito = '0'
016600010330     C                   else
016700010330     C                   if        呃trok > 0
016800010330     C                   eval      wrkesito = '1'
016900010330     C                   else
017000010615     C                   if        flgOk = '0'
017100010615     C                   eval      wrkesito = '2'
017200010615     C                   else
017300010615     C                   eval      wrkesito = '6'
017400010615     C                   endif
017500010330     C                   endif
017600010330     C                   endif
017700010601     C                   else
017800010601     C                   eval      wrkesito = '9'
017900010601     C                   endif
018000010330     C*
018100010330     C                   if        %open(tivin00r)
018200010330     C                   close     tivin00r
018300010330     C                   endif
018400021014     C                   if        %open(fivabwwr)
018500021014     C                   close     fivabwwr
018600010330     C                   endif
018700010601     C*
018800010601     C                   if        vlrpoi <> 999
018900010601     C                   eval      invfil = vlrpoi
019000010601     C                   endif
019100010330     C*
019200010330     C                   if        呃trok > 0
019300010601     C                             and invfil > *zeros
019400010330     C                   exsr      invio
019500010330     C                   endif
019600010601     C*
019700010618     C                   if        flgGiro = '1'
019800010601     C                   exsr      endela
019900010618     C                   endif
020000010330     C*
020100010330     C                   ENDSR
020200010330     C***
020300080321
020400080321
020500080321     C*----------------------------------------------------*
020600080321     C*  SCARICO BUFFER DI OUTPUT
020700080321     C*----------------------------------------------------*
020800080321     C     WRIVAB        BEGSR
020900080321     C*
021000080321     C* Scarico il buffer del FIVAB
021100080321     C                   IF        *IN30 = *on
021200080321     C*
021300080321     C                   EVAL      VABNOT = %trim(%subst(wNOTE:1:35))
021400080321     C                   EVAL      VABNT2 = %trim(%subst(wNOTE:36:35))
021500080321     C*
021600080321     C  N31              WRITE     FIVAB000
021700080321     C                   exsr      inzvar
021800080321     C                   exsr      defcam
021900080321     C                   SETOFF                                       30
022000080321     C                   ENDIF
022100080321     C*
022200080321     C                   ENDSR
022300010330
022400010601
022500010601
022600010601
022700010601
022800010330     C*----------------------------------------------------*
022900020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023000010330     C*----------------------------------------------------*
023100010330     C     INZVAR        BEGSR
023200010330     C*
023300020204     C                   Z-ADD     *zeros        Num5_0            5 0
023400020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
023500020322     C                   MOVEL     '0'           FlgCAS            1
023600080320     C*
023700080320     C                   MOVEL     *blanks       wNOTE            70
023800010330     C*
023900010330     C                   ENDSR
024000010330     C*----------------------------------------------------*
024100020722     C*  IMPOSTAZIONE CAMPI COSTANTI
024200010330     C*----------------------------------------------------*
024300010330     C     DEFCAM        BEGSR
024400010330     C*
024500020204     C* Inizializzo il buffer del record da scrivere
024600021014     C                   CLEAR                   FIVAB000
024700020204     C* Imposto i valori di default...
024800060720     C                   EVAL      VABCCM = 0593024
024900060720     C                   EVAL      VABLNP = 059
025000060720     C                   EVAL      VABCTR = 100
025100040923     C                   EVAL      VABCBO = '1'
025200020204     C* ... e poi verifico se sono stati passati come parametri
025300020204     C                   IF        vlrppt > *blanks
025400040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
025500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025600020204     C                   EXSR      CHKNUM
025700020204     C                   IF        PiInt=*on
025800020204     C                   Z-ADD     PiVal         VABCCM
025900020204     C                   ENDIF
026000040301     C                   ENDIF
026100040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
026200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026300020204     C                   EXSR      CHKNUM
026400020204     C                   IF        PiInt=*on
026500020204     C                   Z-ADD     PiVal         VABLNP
026600020204     C                   ENDIF
026700040301     C                   ENDIF
026800040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
026900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027000020204     C                   EXSR      CHKNUM
027100020204     C                   IF        PiInt=*on
027200020204     C                   Z-ADD     PiVal         VABCTR
027300040301     C                   ENDIF
027400020204     C                   ENDIF
027500020204     C                   ENDIF
027600020204     C*
027700010330     C                   ENDSR
027800010607     C*----------------------------------------------------*
027900021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028000010607     C*----------------------------------------------------*
028100010607     C     IMPVAB        BEGSR
028200010607     C*
028300010607     C                   SETOFF                                       3132
028400060720     C*
028500060720     C* Reperimento campi NUMERICI
028600060720     C                   MOVEL     DATCOR        VABAAS
028700060720     C                   MOVE      DATCOR        VABMGS
028800080321     C*
028900080321     C* ******  TIPO RECORD "1"
029000080321     C                   IF        %subst(vindta:100:1) = '1'
029100080321     C                   IF        wNumSped = *zeros
029200080321     C                   EVAL      wNumSped = 1
029300080321     C                   ELSE
029400080321     C* ...scarico il buffer precedente
029500080321     C*
029600080321     C* Considerazioni finali su CBO/CAS
029700080321     C                   IF        FlgCAS = '1'
029800080321     C                   IF        VABCBO = '1'
029900080321     C                   EVAL      VABCBO = '4'
030000080321     C                   ENDIF
030100080321     C                   IF        VABCBO = '2'
030200080321     C                   EVAL      VABCBO = '6'
030300080321     C                   ENDIF
030400080321     C                   ENDIF
030500080321     C*
030600080321     C* Eseguo routine finale x considerazioni specifiche su importi/divise
030700080321     C                   EXSR      CHKIMPDIV
030800080321     C*
030900080321     C* Quindi scarico il buffer cos� valorizzato
031000080321     C                   SETON                                        30
031100080321     C                   EXSR      WRIVAB
031200080321     C                   ENDIF
031300080321     C                   ENDIF
031400060720     C*
031500060720     C* ******  TIPO RECORD "2"
031600060720     C                   IF        %subst(vindta:100:1) = '2'
031700010607     C*
031800010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
031900060720     C                   EVAL      VABRSD=%trim(%subst(vindta:3:25))
032000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'� diventa A ==
032100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032200020117     C* ==
032300100304     C                   EVAL      VABIND=%trim(%subst(vindta:28:25))
032400100304     C                   EVAL      VABLOD=%trim(%subst(vindta:58:25))
032500100304     C                   EVAL      VABPRD=%trim(%subst(vindta:83:2))
032600060720     C* CAD
032700100304     C                   EVAL      PiStr=%trim(%subst(vindta:53:5))
032800060720     C                   EXSR      CHKNUM
032900060720     C                   IF        PiInt=*on
033000060720     C                   Z-ADD     PiVal         Num5_0
033100060720     C                   MOVEL(p)  Num5_0        VABCAD
033200060720     C                   ELSE
033300060720     C                   SETON                                        32
033400060720     C                   EVAL      VABCAD = *zeros
033500060720     C                   EVAL      vinmsg = %trimr(vinmsg)
033600060720     C                             + ' ' + 'VABCAD'
033700060720     C                   ENDIF
033800060720     C                   ENDIF
033900080219     C*
034000080219     C* ******  TIPO RECORD "NOTE"
034100080320     C                   IF        %subst(vindta:1:7) = 'LONota:'
034200080320     C*
034300080320     C                   select
034400080320     C* DCR
034500080320     C                   when      %subst(vindta:1:23) =
034600080320     C                             'LONota: data consegna: '
034700080320     C                   EVAL      PiStr=%trim(%subst(vindta:24:8))
034800080320     C                   EXSR      CHKNUM
034900080320     C                   IF        PiInt=*on
035000080320     C                   Z-ADD     PiVal         VABDCR
035100080320     C                   ELSE
035200080320     C                   SETON                                        32
035300080320     C                   Z-ADD     *zeros        VABDCR
035400080320     C                   EVAL      vinmsg = %trimr(vinmsg)
035500080320     C                             + ' ' + 'VABDCR'
035600080320     C                   ENDIF
035700080320     C* TSP
035800100823     C***                when      %trim(%subst(vindta:1:19)) =
035900100823     C***                          'LONota: URGENTE'
036000100823     C***                EVAL      VABTSP = 'E'
036100100823     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + 'URGENTE'
036200100211     C                   when      %trim(%subst(vindta:1:19)) =
036300100211     C                             'LONota: PRIORITY'
036400100211     C                   EVAL      VABTSP = 'E'
036500100211     C                   when      %trim(%subst(vindta:1:19)) =
036600100211     C                             'LONota: H10:30'
036700100211     C                   EVAL      VABTSP = 'H'
036800080320     C* CAS
036900150304     C***                when      %subst(vindta:1:21) =
037000150304     C***                          'LONota: CONTRASSEGNO '
037100150304     C***                MOVEL     '1'           FlgCAS
037200150304     C***                EVAL      PiStr=%trim(%subst(vindta:22:20))
037300150304     C***                EXSR      CHKNUM
037400150304     C***                IF        PiNum=*on
037500150304     C***                Z-ADD     PiVal         VABCAS
037600150304     C***                EVAL      VABVCA = 'EUR'
037700150304     C***                EVAL      VABTIC = 'BM'
037800150304     C***                ELSE
037900150304     C***                SETON                                        32
038000150304     C***                Z-ADD     *zeros        VABCAS
038100150304     C***                EVAL      vinmsg = %trimr(vinmsg)
038200150304     C***                          + ' ' + 'VABCAS'
038300150304     C***                ENDIF
038400080320     C* GC1/GC2
038500080320     C                   when      %subst(vindta:1:26) =
038600080320     C                             'LONota: TURNO DI CHIUSURA '
038700080320     C*
038800080320     C                   IF        %subst(vindta:27:2) <> *blanks
038900080320     C                   EVAL      %subst(VABGC1:1:1)  = %subst(vindta:27:1)
039000080320     C                   IF        %subst(vindta:28:1) = 'T'
039100080320     C                   ELSE
039200080320     C                   EVAL      %subst(VABGC1:2:1)  = %subst(vindta:28:1)
039300080320     C                   ENDIF
039400080320     C                   ENDIF
039500080320     C*
039600080320     C                   IF        %subst(vindta:30:2) <> *blanks
039700080320     C                   EVAL      %subst(VABGC2:1:1)  = %subst(vindta:30:1)
039800080320     C                   IF        %subst(vindta:31:1) = 'T'
039900080320     C                   ELSE
040000080320     C                   EVAL      %subst(VABGC2:2:1)  = %subst(vindta:31:1)
040100080320     C                   ENDIF
040200080320     C                   ENDIF
040300080320     C* TC1
040400080321     C*                  when      %subst(vindta:1:40) =
040500080321     C*                            'LONota: TELEFONARE PRIMA DELLA CONSEGNA '
040600080321     C*                  EVAL      VABTC1 = 'A'
040700080321     C*                  EVAL      wNOTE = %trim(wNOTE) + ' ' +
040800080321     C*                                    %trim(%subst(vindta:41:50))
040900150304     C* TC1
041000150304     C                   when      %subst(vindta:101:1) = 'A'
041100150304     C                   EVAL      VABTC1 = 'A'
041200080320     C* NOT/NT2
041300080320     C                   other
041400080320     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
041500080320     C                                     %trim(%subst(vindta:9:50))
041600080320     C*
041700080320     C                   Endsl
041800080219     C*
041900080219     C                   ENDIF
042000060720     C*
042100060720     C* ******  TIPO RECORD "3"
042200060720     C                   IF        %subst(vindta:100:1) = '3'
042300060720     C                   EVAL      VABRMA=%trim(%subst(vindta:3:10))
042400040420     C* NSP/RMN
042500060720     C                   EVAL      PiStr=%trim(%subst(vindta:47:5))
042600020304     C                   EXSR      CHKNUM
042700020304     C                   IF        PiInt=*on
042800040301     C                   Z-ADD     PiVal         VABNSP
042900040301     C                   Z-ADD     PiVal         VABRMN
043000020304     C                   ELSE
043100020722     C                   SETON                                        31
043200020304     C                   Z-ADD     *zeros        VABNSP
043300040301     C                   Z-ADD     *zeros        VABRMN
043400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
043500040301     C                             + ' ' + 'VABNSP VABRMN'
043600020304     C                   ENDIF
043700040420     C* NCL
043800060720     C                   EVAL      PiStr=%trim(%subst(vindta:13:4))
043900010607     C                   EXSR      CHKNUM
044000010607     C                   IF        PiInt=*on
044100010607     C                   Z-ADD     PiVal         VABNCL
044200010607     C                   ELSE
044300010607     C                   SETON                                        32
044400010607     C                   Z-ADD     *zeros        VABNCL
044500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044600010607     C                             + ' ' + 'VABNCL'
044700010607     C                   ENDIF
044800040420     C* PKB
044900060720     C                   EVAL      PiStr=%trim(%subst(vindta:17:7))
045000010607     C                   EXSR      CHKNUM
045100010607     C                   IF        PiNum=*on
045200060720     C                   EVAL(H)   PiVal = PiVal / 100                          * gestisco 2 dec.
045300040420     C                   Z-ADD     PiVal         VABPKB
045400010607     C                   ELSE
045500010607     C                   SETON                                        32
045600010607     C                   Z-ADD     *zeros        VABPKB
045700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
045800010607     C                             + ' ' + 'VABPKB'
045900010607     C                   ENDIF
046000060720     C* IAS
046100060720     C                   IF        %subst(vindta:59:9) <> '000000000'
046200060720     C                   EVAL      VABVAS='EUR'
046300060720     C                   EVAL      PiStr=%trim(%subst(vindta:59:9))
046400060720     C                   EXSR      CHKNUM
046500060720     C                   IF        PiNum=*on
046600060720     C                   Z-ADD     PiVal         VABIAS
046700060720     C                   ELSE
046800060720     C                   SETON                                        32
046900060720     C                   Z-ADD     *zeros        VABIAS
047000060720     C                   EVAL      vinmsg = %trimr(vinmsg)
047100060720     C                             + ' ' + 'VABIAS'
047200060720     C                   ENDIF
047300060720     C                   ENDIF
047400150304     C* CAS
047500150311     C                   IF        %subst(vindta:70:14) <> '0000000000,000' and
047600150311     C                             %subst(vindta:70:14) <> *blank           and
047700150311     C                             %subst(vindta:70:14) <> *zeros
047800150305     C                   MOVEL     '1'           FlgCAS
047900150304     C                   EVAL      VABTIC=%subst(vindta:68:2)
048000150304     C                   EVAL      VABVCA='EUR'
048100150304     C                   EVAL      PiStr=%trim(%subst(vindta:70:14))
048200150304     C                   EXSR      CHKNUM
048300150304     C                   IF        PiNum=*on
048400150304     C                   Z-ADD     PiVal         VABCAS
048500150304     C                   ELSE
048600150304     C                   SETON                                        32
048700150304     C                   Z-ADD     *zeros        VABCAS
048800150304     C                   EVAL      vinmsg = %trimr(vinmsg)
048900150304     C                             + ' ' + 'VABCAS'
049000150304     C                   ENDIF
049100150304     C                   ENDIF
049200150304     C*
049300080320     C                   ENDIF
049400020204     C*
049500010607     C                   ENDSR
049600010607     C*----------------------------------------------------*
049700010601
049800020204
049900020204     C*----------------------------------------------------*
050000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050100020204     C*----------------------------------------------------*
050200020204     C     CHKIMPDIV     BEGSR
050300020204     C*
050400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilit�
050500020204     C                   Z-ADD     *zeros        wrkDec            9 9
050600020204     C*
050700020204     C* Come prima cosa effettuo considerazioni sulla divisa
050800020204     C                   IF        vabIAS > *zeros
050900020204     C                   IF        vabVAS <> 'EUR'
051000020204     C                   EVAL      vabVAS =  'ITL'
051100020204     C                   ENDIF
051200020204     C                   ENDIF
051300020204     C*
051400020204     C                   IF        vabCAS > *zeros
051500020204     C                   IF        vabVCA <> 'EUR'
051600020204     C                   EVAL      vabVCA =  'ITL'
051700020204     C                   ENDIF
051800020204     C                   ENDIF
051900020204     C*
052000020204     C                   IF        vabVMD > *zeros
052100020321     C                   IF        vabVAD <> 'EUR'
052200020204     C                   EVAL      vabVAD =  'ITL'
052300020204     C                   ENDIF
052400020204     C                   ENDIF
052500020204     C*
052600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052700020204     C                   Z-ADD     vabIAS        wrkDec
052800020204     C                   IF        wrkDec > *zeros
052900020204     C                   IF        vabVAS = 'ITL'
053000020204     C                   EVAL      vabIAS = *zeros
053100020204     C                   ENDIF
053200020204     C                   ENDIF
053300020204     C*
053400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
053500020204     C                   Z-ADD     vabCAS        wrkDec
053600020204     C                   IF        wrkDec > *zeros
053700020204     C                   IF        vabVCA = 'ITL'
053800020204     C                   EVAL      vabCAS = *zeros
053900020204     C                   ENDIF
054000020204     C                   ENDIF
054100020204     C*
054200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054300020204     C                   Z-ADD     vabVMD        wrkDec
054400020204     C                   IF        wrkDec > *zeros
054500020204     C                   IF        vabVAD = 'ITL'
054600020204     C                   EVAL      vabVMD = *zeros
054700020204     C                   ENDIF
054800020204     C                   ENDIF
054900020204     C*
055000020204     C                   ENDSR
055100020204     C***
055200020204
055300010330
055400010330
055500010330     C*----------------------------------------------------*
055600010330     C*  CONTROLLO NUMERICITA' CAMPI
055700010330     C*----------------------------------------------------*
055800010330     C     CHKNUM        BEGSR
055900010330     C*
056000010606     C                   IF        PiDecChr = *blanks
056100010606     C                   EVAL      PiDecChr = ','
056200010606     C                   ENDIF
056300010606     C*
056400010606     C                   CALL(e)   'ISNUMERIC'
056500010330     C                   PARM                    PiStr            30
056600010606     C                   PARM                    PiDecChr          1
056700010330     C                   PARM      *ZEROS        PiVal            30 9
056800010330     C                   PARM      '0'           PiInt             1
056900010330     C                   PARM      '0'           PiNum             1
057000010330     C                   IF        %error
057100010606     C                   EVAL      PiNum=*off
057200010330     C                   ENDIF
057300010330     C*
057400010330     C                   ENDSR
057500010330     C***
057600010330
057700010601
057800010601
057900010601
058000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
058100010601     C     repfil        BEGSR
058200010601     C*
058300010601     C                   if        invfil = *zeros and
058400010601     C                             depfil > *zeros and
058500010629     C                             (vinflg = *blanks or
058600010629     C                              vinflg = *zeros)
058700010601     C
058800010601     C                   eval      invfil = depfil
058900010601     C                   endif
059000010601     C*
059100010601     C                   if        depfil <> invfil and
059200010601     C                             invfil > *zeros
059300010601     C                   eval      flgMulti = '1'
059400010601     C                   if        vinflg = *blanks
059500010601     C                   add       1             cntNonEl
059600010601     C                   endif
059700010601     C                   endif
059800010601     C*
059900010601     C                   if        vinflg = '2'
060000010601     C                   eval      flgStato = '2'
060100010601     C                   endif
060200010601     C*
060300010601     C                   ENDSR
060400010601     C***
060500010601
060600010601
060700010601
060800010330
060900010330
061000010330
061100990920      /TITLE Invio dei dati al punto operativo.
061200000613     C     invio         BEGSR
061300990920     C*
061400990920     C                   reset                   dscmz
061500010601     C                   move      invfil        cmzdst
061600990920     C                   eval      cmzfld = vlrfou
061700990920     C                   eval      cmzmbd = vlrhdl
061800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
061900000710     C                   if        prmfir = *blanks
062000021014     C                   eval      cmzfla = 'FIVAB00F'
062100021014     C                   eval      cmzmba = 'FIVAB00F'
062200000710     C                   else
062300000710     C                   eval      cmzfla = prmfir
062400000710     C                   eval      cmzmba = prmfir
062500000710     C                   endif
062600990920     C                   eval      cmznrr = *zeros
062700990920     C                   move      呃trok        cmznrr
062800021018     C                   eval      cmzlba = vlrfl1
062900990920     C                   call(e)   'TIS711C'
063000990920     C                   parm                    dscmz
063100990921     C                   parm      *blanks       esito
063200990920     C                   if        %error
063300990920     C                             or cmzerr = '1'
063400990921     C                             or esito  = '1'
063500000710     C                   eval      wrkesito = '3'
063600990920     C                   endif
063700990920     C*
063800000613     C                   ENDSR
063900990910
064000010601
064100010601
064200010601
064300010601
064400010601      /TITLE Invio dei dati al punto operativo.
064500010601     C     opeini        BEGSR
064600010601     C*
064700010601     C* Inizializzo flag e contatori operativi
064800010601     C                   movel     '0'           flgGiro           1
064900010601     C                   movel     '0'           flgMulti          1
065000010601     C                   movel     '1'           flgStato          1
065100010615     C                   movel     '0'           flgOk             1
065200010601     C                   z-add     *zeros        cntNonEl         10 0
065300010601     C                   z-add     *zeros        depfil            3 0
065400010601     C                   z-add     *zeros        invfil            3 0
065500010601     C*
065600010601     C                   ENDSR
065700010601     C***
065800010601
065900010601
066000010601
066100010330
066200010330
066300000613     C     *inzsr        BEGSR
066400990910     C*
066500990910     C     *entry        plist
066600990920     C                   parm                    tivlrds
066700990921     C                   parm      wrkesito      esito
066800000724     C                   parm                    prmlit
066900000710     C                   parm                    prmfir
067000010330     C*
067100010330     C* CALCOLA LA DATA CORRENTE
067200010330     C                   time                    wn14             14 0
067300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067400010330     C                   z-add     wn8           g08dat
067500010330     C                   z-add     *zeros        g08inv
067600010330     C                   movel     '0'           g08err
067700010330     C                   call      'XSRDA8'
067800010330     C                   parm                    wlbda8
067900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068000000613     C*
068100000613     C                   ENDSR
068200000613     C***
