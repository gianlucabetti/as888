000100080520      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600080520     FEDIVABwr  O    E             DISK    usropn
000700080520     FEDIVATwr  O    E             DISK    usropn
000800070214     Ftitvy0p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100070214     Ftitvy0ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000512     D*------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300990920     D tivlrds       e ds                  extname(tivlr00f)
002400990910     D esito           s              1
002500000724     D prmlit          s             10
002600000710     D prmfir          s             10
002700990921     D wrkesito        s                   like(esito)
002800990915     D wrkdata         s               d
002900990915     D wrkora          s               t
003000000613     D rrnum           s              6  0 INZ(*zeros)
003100000621     D recko           s            150    INZ(*blanks)
003200011123     D depcmd          s            150    INZ(*blanks)
003300050209     D parccm          s              8    INZ(*blanks)
003400050209     D parmbr          s             10    INZ(*blanks)
003500050209     D paropz          s              1    INZ(*blanks)
003600050209     D chkcall         s              1    INZ(*blanks)
003700070214     D*------------------
003800070214     D* DS X RIDEFINIZIONE "ORIZZONTALE" BARCODE COLLI SINGOLA SPEDIZIONE
003900070214     D*------------------
004000070214     D DSWHOIS         DS
004100070214     D  WHOIS01                      20
004200070214     D  FILL001                       1
004300070214     D  WHOIS02                      20
004400070214     D  FILL002                       1
004500070214     D  WHOIS03                      20
004600070214     D  FILL003                       1
004700070214     D  WHOIS04                      20
004800070214     D  FILL004                       1
004900070214     D  WHOIS05                      20
005000070214     D  FILL005                       1
005100070214     D  WHOIS06                      20
005200070214     D  FILL006                       1
005300070214     D  WHOIS07                      20
005400070214     D  FILL007                       1
005500070214     D  WHOIS08                      20
005600070214     D  FILL008                       1
005700070214     D  WHOIS09                      20
005800070214     D  FILL009                       1
005900070214     D  WHOIS10                      20
006000070214     D  FILL010                       1
006100070214     D  WHOIS11                      20
006200070214     D  FILL011                       1
006300070214     D  WHOIS12                      20
006400070214     D  FILL012                       1
006500070214     D  WHOIS13                      20
006600070214     D  FILL013                       1
006700070214     D  WHOIS14                      20
006800070214     D  FILL014                       1
006900070214     D  WHOIS15                      20
007000070214     D  FILL015                       1
007100070214     D  WHOIS16                      20
007200070214     D  FILL016                       1
007300070214     D  WHOIS17                      20
007400070214     D  FILL017                       1
007500070214     D  WHOIS18                      20
007600070214     D  FILL018                       1
007700070214     D  WHOIS19                      20
007800070214     D  FILL019                       1
007900070214     D  WHOIS20                      20
008000070214     D  FILL020                       1
008100010606     D*------------------
008200010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
008300010606     D*------------------
008400010606     D WLBDA8          DS                  INZ
008500010606     D  G08DAT                 1      8  0
008600010606     D  G08INV                 9     16  0
008700010606     D  G08ERR                17     17
008800010606     D  G08TGI                18     22  0
008900050209     D*------------------
009000050209     D* DS ARCHITETTURA
009100050209     D*------------------
009200050209     D kpjba         e ds                  inz
009300050209     D*------------------
009400990908
009500050209
009600050209
009700050209     C* /MAIN
009800990915     C                   time                    wrkdata
009900990915     C                   time                    wrkora
010000000913     C                   reset                   rrnum
010100990921     C                   reset                   esito
010200990921     C                   reset                   wrkesito
010300010530     C*
010400010530     C                   exsr      opeini
010500010530     C                   exsr      rwvab
010600050209     C*
010700050209     C* Effettuo la chiamata al CLLE preposto
010800080716     C                   call(e)   'TITVEVTC'
010900050209     C                   parm                    parccm
011000050209     C                   parm                    parmbr
011100050209     C                   parm      '2'           paropz
011200010530     C*
011300010530     C                   seton                                        lr
011400010530
011500010530
011600010530
011700010530
011800010530     C*--------------------------------------------------------
011900010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012000010530     C*--------------------------------------------------------
012100010530     C     PREELA        BEGSR
012200000724     C*
012300000724     C* SE OCCORRE SPEDIRE IN FILIALE
012400010530     C                   if        invfil <> *zeros and
012500010530     C                             flgGiro = '0'
012600010530     C*
012700010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012800010530     C                   eval      flgGiro = '1'
012900000724     C*
013000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013200010530     C     invfil        chain     azorg01l
013300000724     C                   if        %found
013400000616     C                   movel(p)  CMD(1)        depcmd
013500020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013600000616     C*
013700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013800011123     C                   Z-ADD     150           LENGH            15 5
013900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014000000616     C                   PARM                    depcmd
014100000616     C                   PARM                    LENGH
014200000724     C*
014300000724     C                   endif
014400000616     C*
014500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014600000616     C                   movel(p)  CMD(2)        depcmd
014700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014800000616     C*
014900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015000011123     C                   Z-ADD     150           LENGH            15 5
015100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015200000616     C                   PARM                    depcmd
015300000616     C                   PARM                    LENGH
015400000616     C*
015500070214     C                   if        not %open(titvy0ps)
015600070214     C                   open      titvy0ps
015700030407     C   50              except    testdett
015800000613     C                   endif
015900010530     C*
016000010530     C                   endif
016100010530     C*
016200010530     C                   ENDSR
016300010530     C***
016400010530
016500010530
016600010530
016700010530
016800010530
016900010530
017000010530     C*--------------------------------------------------------
017100010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017200010530     C*--------------------------------------------------------
017300010530     C     ENDELA        BEGSR
017400010530     C*
017500010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017600000613     C*
017700070214     C                   if        %open(titvy0ps)
017800030407     C   50              except    findett
017900070214     C                   close     titvy0ps
018000000613     C                   endif
018100000616     C*
018200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018300011123     C                   Z-ADD     150           LENGH            15 5
018400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018500000616     C                   PARM                    CMD(3)
018600000616     C                   PARM                    LENGH
018700010530     C*
018800010530     C                   ENDSR
018900010530     C***
019000000613
019100010530
019200010530
019300000613
019400000613     C*--------------------------------------------------------
019500000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019600000613     C*--------------------------------------------------------
019700000621     C     STPR          BEGSR
019800000613     C*
019900070214     C                   if        not %open(titvy0p)
020000070214     C                   open      titvy0p
020100990915     C                   endif
020200990915     C*
020300030407     C   50              except    riepilogo
020400990915     C*
020500070214     C                   if        %open(titvy0p)
020600070214     C                   close     titvy0p
020700990914     C                   endif
020800990910     C*
020900000613     C                   ENDSR
021000000613     C***
021100070214
021200070214
021300070214
021400070214     C*--------------------------------------------------------
021500080520     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
021600070214     C*--------------------------------------------------------
021700070214     C     RWVAB         BEGSR
021800070214     C*
021900070214     C                   if        not %open(tivin00r)
022000070214     C                   open      tivin00r
022100070214     C                   endif
022200080520     C                   if        not %open(EDIVABwr)
022300080520     C                   open      EDIVABwr
022400070214     C                   endif
022500070214     C*
022600080520     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
022700070214     C                   exsr      prevat
022800070214     C*
022900070214     C                   if        chkcall = '0'
023000070214     C*
023100080520     C                   if        not %open(EDIVATwr)
023200080520     C                   open      EDIVATwr
023300070214     C                   endif
023400070214     C*
023500070214     C                   clear                   §CTROK
023600070214     C                   clear                   §CTRMO
023700070214     C                   clear                   §CTRNO
023800070214     C*
023900070214     C                   DO        *HIVAL
024000070214     C*
024100070214     C                   READ      tivin00r                               70
024200070214     C*
024300070214     C* Dopo ogni lettura verifico se ci sono stati record OK
024400070214     C                   if        vinflg = '1'
024500070214     C                   eval      flgOk = '1'
024600070214     C                   endif
024700070214     C*
024800070214     C                   if        vindta > *blanks
024900070214     C                   add       1             rrnum
025000070214     C*
025100070214     C                   if        *in70 = *off and
025200070214     C                             (vinflg = *blanks
025300070214     C                              or vinflg = '0'
025400070214     C                              or vinflg = '2')
025500070214     C*
025600070214     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025700070214     C                   if        vinflg = *blanks or vinflg = '0'
025800070214     C                   clear                   vinmsg
025900070214     C                   endif
026000070214     C*
026100070214     C                   exsr      inzvar
026200070214     C                   exsr      defcam
026300070214     C                   exsr      impvab
026400070214     C*
026500070214     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026600070214     C                   eval      depfil = VABLNP
026700070214     C                   exsr      repfil
026800070214     C                   if        depfil = invfil
026900070214     C                   if        vlrpoi = 999
027000070214     C                   MOVE(P)   invfil        VABFGS
027100070214     C                   else
027200070214     C                   MOVE(P)   vlrpoi        VABFGS
027300070214     C                   endif
027400070214     C*
027500070214     C                   exsr      PREELA
027600070214     C*
027700070214     C* Ebbene...
027800070214     C*
027900070214     C  N31              ADD       1             §CTROK            7 0
028000070214     C   32              ADD       1             §CTRMO            7 0
028100070214     C   31              ADD       1             §CTRNO            7 0
028200070214     C*
028300080520     C* Scarico il buffer del EDIVAB
028400080520     C  N31              WRITE     EDIVAB00
028500070214     C  N31              EXSR      WRIVAT
028600070214     C*
028700070214     C                   endif
028800070214     C*
028900070214     C                   if        *in31 = *off and
029000070214     C                             *in32 = *off
029100070214     C                   eval      vinflg = '1'
029200070214     C                   else
029300070214     C                   eval      recko = vindta
029400070214     C                   except    rigadett
029500070214     C                   eval      vinflg = '2'
029600070214     C                   endif
029700070214     C                   endif
029800070214     C*
029900070214     C                   else
030000070214     C                   eval      vinflg = '1'
030100070214     C                   endif
030200070214     C*
030300070214     C  N70              update    tivin000
030400070214     C*
030500070214     C  N70              ENDdo
030600070214     C*
030700070214     C                   endif                                                  <= prevat
030800070214     C*
030900070214     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031000070214     C                   if        cntNonEl = *zeros or
031100070214     C                             flgMulti = '0'
031200070214     C* Se non ci sono record con errori ...
031300070214     C                   if        §ctrno = 0 and
031400070214     C                             §ctrmo = 0 and
031500070214     C                             flgStato <> '2'
031600070214     C* ... restituisco esito OK.
031700070214     C                   eval      wrkesito = '0'
031800070214     C                   else
031900070214     C                   if        §ctrok > 0
032000070214     C                   eval      wrkesito = '1'
032100070214     C                   else
032200070214     C                   if        flgOk = '0'
032300070214     C                   eval      wrkesito = '2'
032400070214     C                   else
032500070214     C                   eval      wrkesito = '6'
032600070214     C                   endif
032700070214     C                   endif
032800070214     C                   endif
032900070214     C                   else
033000070214     C                   eval      wrkesito = '9'
033100070214     C                   endif
033200070214     C*
033300070214     C                   if        %open(tivin00r)
033400070214     C                   close     tivin00r
033500070214     C                   endif
033600080520     C                   if        %open(EDIVABwr)
033700080520     C                   close     EDIVABwr
033800070214     C                   endif
033900080520     C                   if        %open(EDIVATwr)
034000080520     C                   close     EDIVATwr
034100070214     C                   endif
034200070214     C*
034300070214     C                   if        vlrpoi <> 999
034400070214     C                   eval      invfil = vlrpoi
034500070214     C                   endif
034600070214     C*
034700070214     C                   if        §ctrok > 0
034800070214     C                             and invfil > *zeros
034900070214     C                   exsr      invio
035000070214     C                   endif
035100070214     C*
035200070214     C                   if        flgGiro = '1'
035300070214     C                   exsr      endela
035400070214     C                   endif
035500070214     C*
035600070214     C                   ENDSR
035700070214     C***
035800050209
035900070214
036000070214
036100070214     C*----------------------------------------------------*
036200070214     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036300070214     C*----------------------------------------------------*
036400070214     C     INZVAR        BEGSR
036500070214     C*
036600070214     C                   Z-ADD     *zeros        Num5_0            5 0
036700070214     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
036800070214     C                   MOVEL     '0'           FlgCAS            1
036900070214     C*
037000070214     C                   ENDSR
037100070214     C*----------------------------------------------------*
037200070214     C*  IMPOSTAZIONE CAMPI COSTANTI
037300070214     C*----------------------------------------------------*
037400070214     C     DEFCAM        BEGSR
037500070214     C*
037600070214     C* Inizializzo il buffer del record da scrivere
037700080520     C                   CLEAR                   EDIVAB00
037800080520     C                   CLEAR                   EDIVAT00
037900070214     C* Imposto i valori di default...
038000070214     C                   EVAL      VABCCM = 0593027
038100070214     C                   EVAL      VATCCM = 0593027
038200070214     C                   EVAL      VABLNP = 059
038300070214     C                   EVAL      VATLNP = 059
038400070214     C                   EVAL      VABCTR = 000
038500070214     C                   EVAL      VABCBO = '1'
038600070214     C                   EVAL      VABCTM = '7Q'
038700070214     C                   EVAL      VABTSP = 'C'
038800080520     C*
038900080520     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
039000080520     C                   EVAL      VABCMR = 'MAZZOLENI'
039100080520     C                   EVAL      VABDCM = DATCOR
039200080520     C                   EVAL      VABDTS = DATCOR
039300080520     C                   EVAL      VABHMS = ORACOR
039400080520     C                   EVAL      VABCNT = 1
039500080520     C*
039600070214     C* ... e poi verifico se sono stati passati come parametri
039700070214     C                   IF        vlrppt > *blanks
039800070214     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
039900070214     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
040000070214     C                   EXSR      CHKNUM
040100070214     C                   IF        PiInt=*on
040200070214     C                   Z-ADD     PiVal         VABCCM
040300070214     C                   Z-ADD     PiVal         VATCCM
040400070214     C                   ENDIF
040500070214     C                   ENDIF
040600070214     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
040700070214     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
040800070214     C                   EXSR      CHKNUM
040900070214     C                   IF        PiInt=*on
041000070214     C                   Z-ADD     PiVal         VABLNP
041100070214     C                   Z-ADD     PiVal         VATLNP
041200070214     C                   ENDIF
041300070214     C                   ENDIF
041400070214     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
041500070214     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
041600070214     C                   EXSR      CHKNUM
041700070214     C                   IF        PiInt=*on
041800070214     C                   Z-ADD     PiVal         VABCTR
041900070214     C                   ENDIF
042000070214     C                   ENDIF
042100070214     C                   ENDIF
042200070214     C*
042300070214     C                   ENDSR
042400070214     C*----------------------------------------------------*
042500080520     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
042600070214     C*----------------------------------------------------*
042700070214     C     IMPVAB        BEGSR
042800070214     C*
042900070214     C                   SETOFF                                       3132
043000070214     C*
043100070214     C* Reperimento campi ALFA
043200070214     C*
043300070214     C* Considerazioni sulla ragione sociale del destinatario da indicare
043400071023     C                   EVAL      VABRSD=%trim(%subst(vindta:169:35))
043500070214     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043600070214     C     '@':'A'       XLATE     VABRSD        VABRSD
043700070214     C* ==
043800071023     C                   EVAL      VABIND=%trim(%subst(vindta:204:35))
043900071023     C                   EVAL      VABLOD=%trim(%subst(vindta:239:25))
044000071023     C                   EVAL      VABPRD=%trim(%subst(vindta:264:2))
044100070214     C                   EVAL      VABNAS=%trim(%subst(vindta:318:30))
044200070214     C                   EVAL      VABRMA=%trim(%subst(vindta:153:7))
044300070214     C*
044400070214     C* Reperimento campi NUMERICI
044500070214     C                   MOVEL     DATCOR        VABAAS
044600070214     C                   MOVEL     DATCOR        VATAAS
044700070214     C                   MOVE      DATCOR        VABMGS
044800070214     C* NSP/RMN
044900070214     C                   EVAL      PiStr=%trim(%subst(vindta:153:7))
045000070214     C                   EXSR      CHKNUM
045100070214     C                   IF        PiInt=*on
045200070214     C                   Z-ADD     PiVal         VABNSP
045300070214     C                   Z-ADD     PiVal         VATNSP
045400070214     C                   Z-ADD     PiVal         VABRMN
045500070214     C                   ELSE
045600070214     C                   SETON                                        31
045700070214     C                   Z-ADD     *zeros        VABNSP
045800070214     C                   Z-ADD     1             VABRMN
045900070214     C                   EVAL      vinmsg = %trimr(vinmsg)
046000070214     C                             + ' ' + 'VABNSP VATNSP VABRMN'
046100070214     C                   ENDIF
046200070214     C* CAD
046300071023     C                   EVAL      PiStr=%trim(%subst(vindta:266:9))
046400070214     C                   EXSR      CHKNUM
046500070214     C                   IF        PiInt=*on
046600070214     C                   Z-ADD     PiVal         Num5_0
046700070214     C                   MOVEL(p)  Num5_0        VABCAD
046800070214     C                   ELSE
046900070214     C                   SETON                                        32
047000070214     C                   EVAL      VABCAD = *zeros
047100070214     C                   EVAL      vinmsg = %trimr(vinmsg)
047200070214     C                             + ' ' + 'VABCAD'
047300070214     C                   ENDIF
047400070214     C* NCL
047500070214     C                   EVAL      PiStr=%trim(%subst(vindta:296:5))
047600070214     C                   EXSR      CHKNUM
047700070214     C                   IF        PiInt=*on
047800070214     C                   Z-ADD     PiVal         VABNCL
047900070214     C                   ELSE
048000070214     C                   SETON                                        32
048100070214     C                   Z-ADD     *zeros        VABNCL
048200070214     C                   EVAL      vinmsg = %trimr(vinmsg)
048300070214     C                             + ' ' + 'VABNCL'
048400070214     C                   ENDIF
048500070214     C* PKB
048600070214     C                   EVAL      PiStr=%trim(%subst(vindta:282:7))
048700070214     C                   EXSR      CHKNUM
048800070214     C                   IF        PiNum=*on
048900070214     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
049000070214     C                   Z-ADD(H)  PiVal         VABPKB
049100070214     C                   ELSE
049200070214     C                   SETON                                        32
049300070214     C                   Z-ADD     *zeros        VABPKB
049400070214     C                   EVAL      vinmsg = %trimr(vinmsg)
049500070214     C                             + ' ' + 'VABPKB'
049600070214     C                   ENDIF
049700070214     C* VLB
049800070214     C                   EVAL      PiStr=%trim(%subst(vindta:275:7))
049900070214     C                   EXSR      CHKNUM
050000070214     C                   IF        PiNum=*on
050100070214     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
050200070214     C                   Z-ADD(H)  PiVal         VABVLB
050300070214     C                   ELSE
050400070214     C                   SETON                                        32
050500070214     C                   Z-ADD     *zeros        VABVLB
050600070214     C                   EVAL      vinmsg = %trimr(vinmsg)
050700070214     C                             + ' ' + 'VABVLB'
050800070214     C                   ENDIF
050900070214     C*
051000070214     C* Considerazioni finali su CBO/CAS
051100070214     C                   IF        FlgCAS = '1'
051200070214     C                   IF        VABCBO = '1'
051300070214     C                   EVAL      VABCBO = '4'
051400070214     C                   ENDIF
051500070214     C                   IF        VABCBO = '2'
051600070214     C                   EVAL      VABCBO = '6'
051700070214     C                   ENDIF
051800070214     C                   ENDIF
051900070214     C*
052000070214     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052100070214     C                   EXSR      CHKIMPDIV
052200070214     C*
052300070214     C                   ENDSR
052400070214     C*----------------------------------------------------*
052500070214
052600070214
052700070214     C*----------------------------------------------------*
052800070214     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052900070214     C*----------------------------------------------------*
053000070214     C     CHKIMPDIV     BEGSR
053100070214     C*
053200070214     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053300070214     C                   Z-ADD     *zeros        wrkDec            9 9
053400070214     C*
053500070214     C* Come prima cosa effettuo considerazioni sulla divisa
053600070214     C                   IF        vabIAS > *zeros
053700070214     C                   IF        vabVAS <> 'EUR'
053800070214     C                   EVAL      vabVAS =  'ITL'
053900070214     C                   ENDIF
054000070214     C                   ENDIF
054100070214     C*
054200070214     C                   IF        vabCAS > *zeros
054300070214     C                   IF        vabVCA <> 'EUR'
054400070214     C                   EVAL      vabVCA =  'ITL'
054500070214     C                   ENDIF
054600070214     C                   ENDIF
054700070214     C*
054800070214     C                   IF        vabVMD > *zeros
054900070214     C                   IF        vabVAD <> 'EUR'
055000070214     C                   EVAL      vabVAD =  'ITL'
055100070214     C                   ENDIF
055200070214     C                   ENDIF
055300070214     C*
055400070214     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055500070214     C                   Z-ADD     vabIAS        wrkDec
055600070214     C                   IF        wrkDec > *zeros
055700070214     C                   IF        vabVAS = 'ITL'
055800070214     C                   EVAL      vabIAS = *zeros
055900070214     C                   ENDIF
056000070214     C                   ENDIF
056100070214     C*
056200070214     C* Stabilisco se il contrasegno ha decimali valorizzati
056300070214     C                   Z-ADD     vabCAS        wrkDec
056400070214     C                   IF        wrkDec > *zeros
056500070214     C                   IF        vabVCA = 'ITL'
056600070214     C                   EVAL      vabCAS = *zeros
056700070214     C                   ENDIF
056800070214     C                   ENDIF
056900070214     C*
057000070214     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057100070214     C                   Z-ADD     vabVMD        wrkDec
057200070214     C                   IF        wrkDec > *zeros
057300070214     C                   IF        vabVAD = 'ITL'
057400070214     C                   EVAL      vabVMD = *zeros
057500070214     C                   ENDIF
057600070214     C                   ENDIF
057700070214     C*
057800070214     C                   ENDSR
057900070214     C***
058000070214
058100070214
058200070214
058300070214     C*----------------------------------------------------*
058400070214     C*  CONTROLLO NUMERICITA' CAMPI
058500070214     C*----------------------------------------------------*
058600070214     C     CHKNUM        BEGSR
058700070214     C*
058800070214     C                   IF        PiDecChr = *blanks
058900070214     C                   EVAL      PiDecChr = '.'
059000070214     C                   ENDIF
059100070214     C*
059200070214     C                   CALL(e)   'ISNUMERIC'
059300070214     C                   PARM                    PiStr            30
059400070214     C                   PARM                    PiDecChr          1
059500070214     C                   PARM      *ZEROS        PiVal            30 9
059600070214     C                   PARM      '0'           PiInt             1
059700070214     C                   PARM      '0'           PiNum             1
059800070214     C                   IF        %error
059900070214     C                   EVAL      PiNum=*off
060000070214     C                   ENDIF
060100070214     C*
060200070214     C                   ENDSR
060300070214     C***
060400050209
060500050209
060600050209     C*----------------------------------------------------*
060700080520     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
060800050209     C*----------------------------------------------------*
060900050209     C     PREVAT        BEGSR
061000050209     C*
061100080520     C* Compongo il nome del membro da dare al EDIVATWR
061200050209     C                   eval      parmbr = vlrhdl
061300050209     C                   movel     'M'           parmbr
061400050209     C                   eval      parccm = %subst(vlrKSC:2:7)
061500050209     C                   eval      paropz = '1'
061600050209     C* Effettuo la chiamata al CLLE preposto
061700080716     C                   call(e)   'TITVEVTC'
061800050209     C                   parm                    parccm
061900050209     C                   parm                    parmbr
062000050209     C                   parm                    paropz
062100050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062200050209     C                   if        %error
062300050209     C                   movel     '1'           chkcall
062400050209     C                   else
062500050209     C                   movel     '0'           chkcall
062600050209     C                   endif
062700050209     C*
062800050209     C                   ENDSR
062900050209
063000050209
063100990920
063200050209     C*----------------------------------------------------*
063300080520     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
063400050209     C*----------------------------------------------------*
063500050209     C     WRIVAT        BEGSR
063600050209     C*
063700050209     C* Valorizzo il buffer del file dettagli barcode colli
063800050209     C                   EVAL      VATFGS = VABFGS
063900050209     C                   EVAL      VATAAS = VABAAS
064000050209     C                   EVAL      VATLNP = VABLNP
064100050209     C                   EVAL      VATNSP = VABNSP
064200050209     C                   EVAL      VATCCM = VABCCM
064300070209     C                   EVAL      VATTRC='E'
064400080520     C*
064500080520     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
064600080520     C                   EVAL      VATCMR = VABCMR
064700080520     C                   EVAL      VATCNT = VABCNT
064800070214     C*
064900070214     C* Ridefinisco il campo con l'elenco orizzonatale dei "chi sono"
065000070214     C                   EVAL      DSWHOIS = %subst(vindta:360:512)
065100070214     C*
065200070214     C* Quindi scarico buffer x ogni id collo presente (max 10 colli x bolla)
065300070214     C                   IF        WHOIS01 <> *blanks
065400070214     C                   EVAL      VATNOT = WHOIS01
065500080520     C                   WRITE     EDIVAT00
065600070214     C                   ENDIF
065700070214     C*
065800070214     C                   IF        WHOIS02 <> *blanks
065900070214     C                   EVAL      VATNOT = WHOIS02
066000080520     C                   WRITE     EDIVAT00
066100070214     C                   ENDIF
066200070214     C*
066300070214     C                   IF        WHOIS03 <> *blanks
066400070214     C                   EVAL      VATNOT = WHOIS03
066500080520     C                   WRITE     EDIVAT00
066600070214     C                   ENDIF
066700070214     C*
066800070214     C                   IF        WHOIS04 <> *blanks
066900070214     C                   EVAL      VATNOT = WHOIS04
067000080520     C                   WRITE     EDIVAT00
067100070214     C                   ENDIF
067200070214     C*
067300070214     C                   IF        WHOIS05 <> *blanks
067400070214     C                   EVAL      VATNOT = WHOIS05
067500080520     C                   WRITE     EDIVAT00
067600070214     C                   ENDIF
067700070214     C*
067800070214     C                   IF        WHOIS06 <> *blanks
067900070214     C                   EVAL      VATNOT = WHOIS06
068000080520     C                   WRITE     EDIVAT00
068100070214     C                   ENDIF
068200070214     C*
068300070214     C                   IF        WHOIS07 <> *blanks
068400070214     C                   EVAL      VATNOT = WHOIS07
068500080520     C                   WRITE     EDIVAT00
068600070214     C                   ENDIF
068700070214     C*
068800070214     C                   IF        WHOIS08 <> *blanks
068900070214     C                   EVAL      VATNOT = WHOIS08
069000080520     C                   WRITE     EDIVAT00
069100070214     C                   ENDIF
069200070214     C*
069300070214     C                   IF        WHOIS09 <> *blanks
069400070214     C                   EVAL      VATNOT = WHOIS09
069500080520     C                   WRITE     EDIVAT00
069600070214     C                   ENDIF
069700070214     C*
069800070214     C                   IF        WHOIS10 <> *blanks
069900070214     C                   EVAL      VATNOT = WHOIS10
070000080520     C                   WRITE     EDIVAT00
070100070214     C                   ENDIF
070200050209     C*
070300050209     C                   ENDSR
070400010530
070500010530
070600050209
070700010530
070800010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
070900010530     C     repfil        BEGSR
071000010530     C*
071100010530     C                   if        invfil = *zeros and
071200010530     C                             depfil > *zeros and
071300010613     C                             (vinflg = *blanks or
071400010613     C                              vinflg = *zeros)
071500010530     C
071600010530     C                   eval      invfil = depfil
071700010530     C                   endif
071800010530     C*
071900010530     C                   if        depfil <> invfil and
072000010530     C                             invfil > *zeros
072100010530     C                   eval      flgMulti = '1'
072200010530     C                   if        vinflg = *blanks
072300010530     C                   add       1             cntNonEl
072400010530     C                   endif
072500010530     C                   endif
072600010530     C*
072700010530     C                   if        vinflg = '2'
072800010530     C                   eval      flgStato = '2'
072900010530     C                   endif
073000010530     C*
073100010530     C                   ENDSR
073200010530     C***
073300050209
073400050209
073500050209
073600050209
073700050209      /TITLE Invio dei dati al punto operativo.
073800050209     C     invio         BEGSR
073900050209     C*
074000080520     C* 1° invio EDIVAT
074100050209     C                   reset                   dscmz
074200050209     C                   move      vlrpoi        cmzdst
074300080520     C                   eval      cmzfld = 'EDIVATWR'
074400050209     C                   eval      cmzmbd = vlrhdl
074500050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
074600050209     C***                if        prmfir = *blanks
074700080520     C                   eval      cmzfla = 'EDIVAT0F'
074800080520     C                   eval      cmzmba = 'EDIVAT0F'
074900050209     C***                else
075000050209     C***                eval      cmzfla = prmfir
075100050209     C***                eval      cmzmba = prmfir
075200050209     C***                endif
075300050209     C                   eval      cmznrr = *zeros
075400050209     C                   move      §ctrok        cmznrr
075500050209     C                   eval      cmzlba = vlrfl1
075600050209     C                   call(e)   'TIS711C'
075700050209     C                   parm                    dscmz
075800050209     C                   parm      *blanks       esito
075900050209     C                   if        %error
076000050209     C                             or cmzerr = '1'
076100050209     C                             or esito  = '1'
076200050209     C                   eval      wrkesito = '3'
076300050209     C                   else
076400050209     C*
076500080520     C* 2° invio EDIVAB
076600050209     C                   reset                   dscmz
076700050209     C                   move      vlrpoi        cmzdst
076800050209     C                   eval      cmzfld = vlrfou
076900050209     C                   eval      cmzmbd = vlrhdl
077000050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
077100050209     C***                if        prmfir = *blanks
077200080520     C                   eval      cmzfla = 'EDIVAB0F'
077300080520     C                   eval      cmzmba = 'EDIVAB0F'
077400050209     C***                else
077500050209     C***                eval      cmzfla = prmfir
077600050209     C***                eval      cmzmba = prmfir
077700050209     C***                endif
077800050209     C                   eval      cmznrr = *zeros
077900050209     C                   move      §ctrok        cmznrr
078000050209     C                   eval      cmzlba = vlrfl1
078100050209     C                   call(e)   'TIS711C'
078200050209     C                   parm                    dscmz
078300050209     C                   parm      *blanks       esito
078400050209     C                   if        %error
078500050209     C                             or cmzerr = '1'
078600050209     C                             or esito  = '1'
078700050209     C                   eval      wrkesito = '3'
078800050209     C                   endif
078900050209     C                   endif
079000050209     C*
079100050209     C                   ENDSR
079200050209     C***
079300010530
079400010530
079500010530
079600010530
079700010530      /TITLE Invio dei dati al punto operativo.
079800010530     C     opeini        BEGSR
079900010530     C*
080000010530     C* Inizializzo flag e contatori operativi
080100010530     C                   movel     '0'           flgGiro           1
080200010530     C                   movel     '0'           flgMulti          1
080300010530     C                   movel     '1'           flgStato          1
080400010615     C                   movel     '0'           flgOk             1
080500010530     C                   z-add     *zeros        cntNonEl         10 0
080600010530     C                   z-add     *zeros        depfil            3 0
080700010530     C                   z-add     *zeros        invfil            3 0
080800030407     C*
080900070214     C                   setoff                                       50
081000010530     C*
081100010530     C                   ENDSR
081200010530     C***
081300010530
081400010530
081500070411
081600070411     C     *pssr         BEGSR
081700070411     C*
081800070411     C                   if        %open(tivin00r)
081900070411     C                   close     tivin00r
082000070411     C                   endif
082100080520     C                   if        %open(EDIVABwr)
082200080520     C                   close     EDIVABwr
082300070411     C                   endif
082400080520     C                   if        %open(EDIVATwr)
082500080520     C                   close     EDIVATwr
082600070411     C                   endif
082700070411     C*
082800070411     C* Effettuo la chiamata al CLLE preposto
082900080716     C                   call(e)   'TITVEVTC'
083000070411     C                   parm                    parccm
083100070411     C                   parm                    parmbr
083200070411     C                   parm      '2'           paropz
083300070411     C*
083400070411     C                   eval      wrkesito = '2'
083500070411     C*
083600070411     C                   seton                                        LR
083700070411     C*
083800070411     C                   ENDSR     '*CANCL'
083900070411     C***
084000070411
084100010530
084200990910
084300000613     C     *inzsr        BEGSR
084400990910     C*
084500990910     C     *entry        plist
084600990920     C                   parm                    tivlrds
084700990921     C                   parm      wrkesito      esito
084800000724     C                   parm                    prmlit
084900000710     C                   parm                    prmfir
085000010606     C*
085100010606     C* CALCOLA LA DATA CORRENTE
085200010606     C                   time                    wn14             14 0
085300010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
085400080520     C                   movel     wn14          oracor            6 0          *ORA
085500010606     C                   z-add     wn8           g08dat
085600010606     C                   z-add     *zeros        g08inv
085700010606     C                   movel     '0'           g08err
085800010606     C                   call      'XSRDA8'
085900010606     C                   parm                    wlbda8
086000010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
086100000613     C*
086200000613     C                   ENDSR
086300000613     C***
086400990908
086500070214     Otitvy0p   E            riepilogo         2
086600990915     O                                              'Upload via Internet'
086700990915     O                                           +1 'Traduzione TIVIN00R -'
086800080520     O                                           +1 'EDIVABWR'
086900990915     O                                           +1 'Testate bolle'
087000990915     O          E            riepilogo   2
087100990915     O                       wrkdata
087200990915     O                       wrkora              +1
087300990915     O                       procname            +1
087400990915     O          E            riepilogo   2
087500990915     O                                              'Cliente..................:'
087600990915     O                       VABCCM        z     +1
087700990915     O          E            riepilogo   2
087800990920     O                                              'Riferimento Strategi.....:'
087900990920     O                       vlrhdl              +1
088000990915     O          E            riepilogo   2
088100990915     O                                              'Giusti...................:'
088200971022     O                       §CTROK        2   +  1
088300990915     O          E            riepilogo   2
088400990915     O                                              'Sbagliati e corretti.....:'
088500971022     O                       §CTRMO        2   +  1
088600990915     O          E            riepilogo   2
088700990915     O                                              'Sbagliati e scartati.....:'
088800971022     O                       §CTRNO        2   +  1
088900000613
089000070214     Otitvy0ps  E            testdett          2
089100000613     O                                              'Upload via Internet'
089200000613     O                                           +1 'Traduzione TIVIN00R -'
089300080520     O                                           +1 'EDIVABWR'
089400000613     O                                           +1 'Testate bolle'
089500000616     O          E            testdett    3
089600000613     O                                           +2 'N° rec'
089700000613     O                                           +3 'Anteprima contenuto'
089800000616     O          E            rigadett    2
089900000613     O                       rrnum               +2
090000000621     O                       recko               +3
090100000616     O          E            findett     2
090200000613     O                       wrkdata
090300000613     O                       wrkora              +1
090400000613     O                       procname            +1
090500000616     O          E            findett     2
090600000613     O                                              'Cliente..................:'
090700000613     O                       VABCCM        z     +1
090800000616     O          E            findett     2
090900000613     O                                              'Riferimento Strategi.....:'
091000000613     O                       vlrhdl              +1
091100000616     O          E            findett     2
091200000613     O                                              'Giusti...................:'
091300000613     O                       §CTROK        2   +  1
091400000616     O          E            findett     2
091500000613     O                                              'Sbagliati e corretti.....:'
091600000613     O                       §CTRMO        2   +  1
091700000616     O          E            findett     2
091800000613     O                                              'Sbagliati e scartati.....:'
091900000613     O                       §CTRNO        2   +  1
092000000512** CMD - COMANDI CL
092100070214OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY0P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
092200070214OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVY0PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
092300070214DLTOVR FILE(TITVY0P TITVY0PS) LVL(*)
092400000512
092500000512
