000100080512      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600080512     FEDIVABwr  O    E             DISK    usropn
000700080512     FEDIVATwr  O    E             DISK    usropn
000800080512     Ftitv1g2p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100080512     Ftitv1g2ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600060307     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600010202     D parccm          s              8    INZ(*blanks)
003700010202     D parmbr          s             10    INZ(*blanks)
003800010202     D paropz          s              1    INZ(*blanks)
003900010202     D chkcall         s              1    INZ(*blanks)
004000080512     D curSped         s              9    INZ(*blanks)
004100080512     D depSped         s              9    INZ(*blanks)
004200080512     D wNomeFile       s             30    INZ(*blanks)
004300000830
004400000830     D*------------------
004500000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004600000830     D*------------------
004700000830     D WLBDA8          DS                  INZ
004800000830     D  G08DAT                 1      8  0
004900000830     D  G08INV                 9     16  0
005000000830     D  G08ERR                17     17
005100000830     D  G08TGI                18     22  0
005200080513     D*------------------
005300080513     D* DS REPERIMENTO NUMERATORE
005400080513     D*------------------
005500080513     D trul33ds      e ds                  inz
005600080513     D*------------------
005700080513     D* DS ARCHITETTURA
005800080513     D*------------------
005900080513     D kpjba         e ds                  inz
006000010201
006100010201
006200990915     C                   time                    wrkdata
006300990915     C                   time                    wrkora
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700000724     C*
006800000724     C* SE OCCORRE SPEDIRE IN FILIALE
006900000724     C                   if        vlrpoi <> *zeros
007000000724     C*
007100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007300000724     C     vlrpoi        chain     azorg01l
007400000724     C                   if        %found
007500000616     C                   movel(p)  CMD(1)        depcmd
007600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007700000616     C*
007800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007900011113     C                   Z-ADD     150           LENGH            15 5
008000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008100000616     C                   PARM                    depcmd
008200000616     C                   PARM                    LENGH
008300000724     C*
008400000724     C                   endif
008500000724     C                   endif
008600000616     C*
008700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000616     C                   movel(p)  CMD(2)        depcmd
008900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009000000616     C*
009100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009200011113     C                   Z-ADD     150           LENGH            15 5
009300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009400000616     C                   PARM                    depcmd
009500000616     C                   PARM                    LENGH
009600000616     C*
009700080512     C                   if        not %open(titv1g2ps)
009800080512     C                   open      titv1g2ps
009900000616     C                   except    testdett
010000000613     C                   endif
010100000613     C*
010200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010400000613     C*
010500010202     C* Effettuo la chiamata al CLLE preposto
010600080512     C                   call(e)   'TITVEVTC'
010700010202     C                   parm                    parccm
010800010202     C                   parm                    parmbr
010900010202     C                   parm      '2'           paropz
011000050201     C*
011100050201     C* Effettuo lancio TISI95 solo x chiusura
011200050201     C                   CLEAR                   TISI95DS
011300050201     C                   EVAL      I95TLA = 'C'
011400050201     C                   CALL      'TISI95R'
011500050201     C                   PARM                    TISI95DS
011600010202     C*
011700080512     C                   if        %open(titv1g2ps)
011800000616     C                   except    findett
011900080512     C                   close     titv1g2ps
012000000613     C                   endif
012100000616     C*
012200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012300011113     C                   Z-ADD     150           LENGH            15 5
012400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    CMD(3)
012600000616     C                   PARM                    LENGH
012700000616     C*
012800000801     C
012900010201     C                   seton                                        LR
013000000613
013100000613
013200000613     C*--------------------------------------------------------
013300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013400000613     C*--------------------------------------------------------
013500000621     C     STPR          BEGSR
013600000613     C*
013700080512     C                   if        not %open(titv1g2p)
013800080512     C                   open      titv1g2p
013900990915     C                   endif
014000990915     C*
014100990915     C                   except    riepilogo
014200990915     C*
014300080512     C                   if        %open(titv1g2p)
014400080512     C                   close     titv1g2p
014500990914     C                   endif
014600990910     C*
014700000613     C                   ENDSR
014800000613     C***
014900990908
015000000801
015100910830     C*--------------------------------------------------------
015200080512     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
015300910830     C*--------------------------------------------------------
015400040526     C     RWFILE        BEGSR
015500990910     C*
015600990914     C                   if        not %open(tivin00r)
015700990908     C                   open      tivin00r
015800990914     C                   endif
015900080512     C                   if        not %open(edivabwr)
016000080512     C                   open      edivabwr
016100990914     C                   endif
016200070103     C*
016300080512     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016400020305     C                   exsr      prevat
016500010201     C*
016600010202     C                   if        chkcall = '0'
016700010202     C*
016800080512     C                   if        not %open(edivatwr)
016900080512     C                   open      edivatwr
017000010201     C                   endif
017100990910     C*
017200010201     C                   clear                   §CTROKVB          5 0
017300020305     C                   clear                   §CTROKVT          5 0
017400000801     C                   clear                   §CTRMO            5 0
017500000801     C                   clear                   §CTRNO            5 0
017600990910     C*
017700921023     C                   DO        *HIVAL
017800990913     C*
017900990915     C                   READ      tivin00r                               70
018000050627     C                   if        vindta > *blanks
018100000613     C                   add       1             rrnum
018200000801     C*
018300000801     C                   if        *in70 = *off
018400000801     C                             and
018500000801     C                             (vinflg = *blanks
018600000801     C                              or vinflg = '0'
018700000801     C                              or vinflg = '2')
018800000801     C*
018900000801     C                   clear                   vinmsg
019000000801     C                   eval      vinflg = '1'
019100070103     C*
019200070103     C* Verifico la rottura d codice x raggruppamento spedizione cliente
019300080512     C                   eval      curSped = %subst(vindta:7:9)
019400070213     C                   if        curSped <> depSped
019500070213     C* Se prima bolla => importo bolla corrente
019600070213     C                   if        §CTRMO = *zeros
019700070213     C                   exsr      inzvar
019800070213     C                   exsr      defcam
019900070213     C                   exsr      impvab                                       => carico  VAB
020000070213     C                   else
020100070213     C* Se no prima bolla => scarico bolla precedente + importo bolla corrente
020200070213     C                   exsr      wrivab                                       => scarico VAB
020300070213     C                   exsr      inzvar
020400070213     C                   exsr      defcam
020500070213     C                   exsr      impvab                                       => carico  VAB
020600070213     C                   endif
020700071121     C                   exsr      exevate                                      => write VAT-E
020800070103     C* Salvo il raggruppamento spedizione cliente corrente
020900070103     C                   eval      depSped = curSped
021000070213     C*
021100070213     C* Se collo successivo x stessa bolla
021200070213     C                   else
021300070213     C                   exsr      impvab                                       => carico  VAB
021400070213     C                   exsr      exevate                                      => write VAT-E
021500070103     C                   endif
021600000905     C*
021700000905     C                   else
021800000905     C                   eval      vinflg = '1'
021900050628     C                   endif
022000000905     C                   endif
022100000905     C*
022200000905     C  N70              update    tivin000
022300000905     C*
022400991022     C  N70              ENDdo
022500070213     C*
022600070213     C* Scarico testata bolla rimasta in sospesa
022700070213     C                   exsr      wrivab                                       => scarico VAB
022800010202     C*
022900010202     C                   endif
023000990910
023100990910     C* Se non ci sono record con errori ...
023200000710     C                   if        §ctrno = 0
023300990910     C* ... restituisco esito OK.
023400990921     C                   eval      wrkesito = '0'
023500990910     C                   else
023600010201     C                   if        §ctrokvb > 0
023700990921     C                   eval      wrkesito = '1'
023800000710     C                   else
023900000710     C                   eval      wrkesito = '2'
024000990910     C                   endif
024100000710     C                   endif
024200990910     C*
024300990914     C                   if        %open(tivin00r)
024400990908     C                   close     tivin00r
024500990914     C                   endif
024600080512     C                   if        %open(edivabwr)
024700080512     C                   close     edivabwr
024800990914     C                   endif
024900080512     C                   if        %open(edivatwr)
025000080512     C                   close     edivatwr
025100010201     C                   endif
025200990910     C*
025300010201     C                   if        §ctrokvb > 0
025400000724     C                             and vlrpoi <> *zeros
025500010202     C                   exsr      invio
025600990920     C                   endif
025700990920     C*
025800910830     C                   ENDSR
025900000613     C***
026000010305
026100010305     C*----------------------------------------------------*
026200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026300010305     C*----------------------------------------------------*
026400020305     C     WRIVAB        BEGSR
026500080512     C*
026600080512     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
026700080512     C                   EVAL      VABCMR = %subst(wNomeFile:10:6)
026800080512     C                   EVAL      VABDCM = DATCOR
026900080512     C                   EVAL      VABDTS = DATCOR
027000080512     C                   EVAL      VABHMS = ORACOR
027100080512     C                   EVAL      VABCNT = 1
027200010305     C*
027300060225     C* Quindi scarico il buffer del file d testata
027400080512     C                   write     edivab00                                     => scarico il VAB
027500010305     C*
027600010305     C                   ENDSR
027700990920
027800000801     C*----------------------------------------------------*
027900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028000000801     C*----------------------------------------------------*
028100010201     C     INZVAR        BEGSR
028200000801     C*
028300080512     C                   CLEAR                   EDIVAB00
028400080512     C                   CLEAR                   EDIVAT00
028500070213     C*
028600040802     C                   Z-ADD     *zeros        Num5_0            5 0
028700040802     C                   MOVEL     '0'           FlgCAS            1
028800000801     C*
028900000801     C                   ENDSR
029000000801     C*----------------------------------------------------*
029100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029200000801     C*----------------------------------------------------*
029300000801     C     DEFCAM        BEGSR
029400000801     C*
029500020619     C* Imposto i valori di default...
029600080512     C                   Z-ADD     0054637       VABCCM
029700080512     C                   Z-ADD     0054637       VATCCM
029800080512     C                   Z-ADD     005           VABLNP
029900080512     C                   Z-ADD     005           VATLNP
030000080512     C                   Z-ADD     100           VABCTR
030100070103     C                   MOVEL     '7Q'          VABCTM
030200040823     C                   MOVEL     '1'           VABCBO
030300080512     C                   Z-ADD     1             VABNCL
030400020619     C* ... e poi verifico se sono stati passati come parametri
030500020619     C                   IF        vlrppt > *blanks
030600040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
030700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030800020619     C                   EXSR      CHKNUM
030900020619     C                   IF        PiInt=*on
031000020619     C                   Z-ADD     PiVal         VABCCM
031100020619     C                   Z-ADD     PiVal         VATCCM
031200020619     C                   ENDIF
031300040506     C                   ENDIF
031400040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
031500020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031600020619     C                   EXSR      CHKNUM
031700020619     C                   IF        PiInt=*on
031800020619     C                   Z-ADD     PiVal         VABLNP
031900020619     C                   Z-ADD     PiVal         VATLNP
032000040506     C                   ENDIF
032100020619     C                   ENDIF
032200040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
032300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032400020619     C                   EXSR      CHKNUM
032500020619     C                   IF        PiInt=*on
032600020619     C                   Z-ADD     PiVal         VABCTR
032700040506     C                   ENDIF
032800020619     C                   ENDIF
032900020619     C                   ENDIF
033000000801     C*
033100000801     C                   ENDSR
033200000801     C*----------------------------------------------------*
033300080512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
033400000801     C*----------------------------------------------------*
033500040823     C     IMPVAB        BEGSR
033600010305     C*
033700000801     C                   Z-ADD     *zeros        errore            1 0
033800000830     C                   MOVEL     datcor        VABAAS
033900020305     C                   MOVEL     datcor        VATAAS
034000040526     C                   MOVE      datcor        VABMGS
034100040823     C                   MOVE(P)   vlrpoi        VABFGS
034200040823     C                   MOVE(P)   vlrpoi        VATFGS
034300080409     C*
034400080409     C* Reperimento campi ALFA
034500080409     C*
034600080409     C* Considerazioni sulla ragione sociale del destinatario da indicare
034700080512     C                   EVAL      VABRSD=%trim(%subst(vindta:17:19))
034800080409     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034900080409     C     '@':'A'       XLATE     VABRSD        VABRSD
035000080409     C* ==
035100080512     C                   EVAL      VABIND=%trim(%subst(vindta:36:30))
035200080512     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
035300080512     C                   EVAL      VABPRD=%trim(%subst(vindta:101:2))
035400080512     C                   EVAL      VABRMA=%trim(%subst(vindta:7:9))
035500080512     C*
035600080512     C* Gestioe particolarità
035700080512     C                   SELECT
035800080512     C                   WHEN      %trim(%subst(vindta:123:2)) ='ST'
035900080512     C                   EVAL      VABCTR=100
036000080512     C                   EVAL      VABPKB=101,0
036100080623     C                   WHEN      %trim(%subst(vindta:123:1)) ='P'
036200080512     C                   EVAL      VABCTR=101
036300080512     C                   EVAL      VABPKB=110,0
036400080512     C                   OTHER
036500080512     C                   EVAL      VABCTR=301
036600080512     C                   EVAL      VABPKB=1,0
036700080512     C                   ENDSL
036800080409     C*
036900080409     C* Reperimento campi NUMERICI
037000080409     C                   MOVEL     DATCOR        VABAAS
037100080409     C                   MOVE      DATCOR        VABMGS
037200080513     C* RMN
037300080512     C                   EVAL      PiStr=%trim(%subst(vindta:7:9))
037400080409     C                   EXSR      CHKNUM
037500080409     C                   IF        PiInt=*on
037600080409     C                   Z-ADD     PiVal         VABRMN
037700080409     C                   ELSE
037800080409     C                   ADD       1             errore
037900080409     C                   Z-ADD     1             VABRMN
038000080409     C                   EVAL      vinmsg = %trimr(vinmsg)
038100080513     C                             + ' ' + 'VABRMN'
038200080409     C                   ENDIF
038300080513     C* NSP => Stacco un numeratore da AZNUM
038400080513     C                   clear                   TRUL33DS
038500080513     C                   eval      I33OPE = *zeros
038600080513     C                   eval      I33CNU = 302
038700080513     C                   eval      I33NUM = 1
038800080513     C                   movel     TRUL33DS      KPJBU
038900080513     C                   call      'TRUL33R'
039000080513     C                   parm                    KPJBA
039100080513     C                   movel     KPJBU         TRUL33DS
039200080513     C                   if        O33ERR = *zeros
039300080513     C                   z-add     O33NRF        VABNSP
039400080513     C                   z-add     O33NRF        VATNSP
039500080513     C                   else
039600080513     C                   Z-ADD     1             errore
039700080513     C                   EVAL      vinmsg = %trimr(vinmsg)
039800080513     C                             + ' ' + 'VABNSP VATNSP'
039900080513     C                   endif
040000080409     C* CAD
040100080512     C                   EVAL      PiStr=%trim(%subst(vindta:66:5))
040200080409     C                   EXSR      CHKNUM
040300080409     C                   IF        PiInt=*on
040400080409     C                   Z-ADD     PiVal         Num5_0
040500080409     C                   MOVEL(p)  Num5_0        VABCAD
040600080409     C                   ELSE
040700080409     C                   ADD       1             errore
040800080409     C                   EVAL      VABCAD = *zeros
040900080409     C                   EVAL      vinmsg = %trimr(vinmsg)
041000080409     C                             + ' ' + 'VABCAD'
041100080409     C                   ENDIF
041200071121     C*
041300080415     C* Carico l'estensioni A e B del FNVAT
041400080512     C                   exsr      exevata
041500080512     C                   exsr      exevatb
041600010205     C*
041700010205     C* Considerazioni sul contenuto di campi precedentemente valorizzati
041800040802     C                   IF        FlgCAS <> '0'
041900010205     C                   EVAL      VABCBO = '4'
042000010205     C                   ENDIF
042100020305     C*
042200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
042300011113     C                   EXSR      CHKIMPDIV
042400010202     C*
042500000801     C* Ebbene...
042600000801     C                   ADD       1             §CTRMO
042700010201     C                   IF        errore <> *zeros
042800000801     C                   ADD       1             §CTRNO
042900000801     C                   EVAL      recko = vindta
043000000801     C                   EXCEPT    rigadett
043100000801     C                   EVAL      vinflg = '2'
043200000801     C                   ELSE
043300010201     C                   ADD       1             §CTROKVB
043400000801     C                   ENDIF
043500000801     C*
043600000801     C                   ENDSR
043700070102     C*----------------------------------------------------*
043800080512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "E"
043900070102     C*----------------------------------------------------*
044000070103     C     EXEVATE       BEGSR
044100070102     C*
044200070103     C                   EVAL      VATTRC='E'
044300080512     C                   EVAL      VATNOT=%trim(%subst(vindta:1:16))
044400080512     C*
044500080512     C                   exsr      wrivat                                       => scarico VAT
044600070102     C*
044700070102     C                   ENDSR
044800080415     C*----------------------------------------------------*
044900080512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "A"
045000080415     C*----------------------------------------------------*
045100080415     C     EXEVATA       BEGSR
045200080415     C*
045300080415     C*
045400080415     C***                exsr      wrivat                                       => scarico VAT
045500080415     C*
045600080415     C                   ENDSR
045700071121     C*----------------------------------------------------*
045800080512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
045900071121     C*----------------------------------------------------*
046000071121     C     EXEVATB       BEGSR
046100071121     C*
046200071121     C*
046300080409     C***                exsr      wrivat                                       => scarico VAT
046400071121     C*
046500071121     C                   ENDSR
046600010201     C*----------------------------------------------------*
046700080512     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
046800010201     C*----------------------------------------------------*
046900020305     C     WRIVAT        BEGSR
047000080512     C*
047100080512     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
047200080512     C                   EVAL      VATCMR = %subst(wNomeFile:10:6)
047300080512     C                   EVAL      VATCNT = 1
047400050628     C*
047500060223     C* Scrivo solo se valorizzato qualcosa
047600060223     C                   IF        VATNOT <> *blanks
047700080512     C                   WRITE     EDIVAT00
047800060223     C                   ENDIF
047900010201     C*
048000010201     C                   ENDSR
048100010202     C*----------------------------------------------------*
048200080512     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
048300010202     C*----------------------------------------------------*
048400020305     C     PREVAT        BEGSR
048500010202     C*
048600080512     C* Compongo il nome del membro da dare al EDIVATWR
048700010202     C                   eval      parmbr = vlrhdl
048800010202     C                   movel     'M'           parmbr
048900050627     C                   eval      parccm = %subst(vlrKSC:2:7)
049000010202     C                   eval      paropz = '1'
049100010202     C* Effettuo la chiamata al CLLE preposto
049200080512     C                   call(e)   'TITVEVTC'
049300010202     C                   parm                    parccm
049400010202     C                   parm                    parmbr
049500010202     C                   parm                    paropz
049600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049700010202     C                   if        %error
049800010202     C                   movel     '1'           chkcall
049900010202     C                   else
050000010202     C                   movel     '0'           chkcall
050100010202     C                   endif
050200010202     C*
050300010202     C                   ENDSR
050400000801     C*----------------------------------------------------*
050500000801     C*  CONTROLLO NUMERICITA' CAMPI
050600000801     C*----------------------------------------------------*
050700000801     C     CHKNUM        BEGSR
050800000801     C*
050900000801     C                   call(e)   'ISNUMERIC'
051000000801     C                   PARM                    PiStr            30
051100070213     C                   PARM      ','           PiDecChr          1
051200000801     C                   PARM      *ZEROS        PiVal            30 9
051300000801     C                   PARM      '0'           PiInt             1
051400000801     C                   PARM      '0'           PiNum             1
051500000801     C                   IF        %error
051600000801     C                   EVAL      PiInt=*off
051700000801     C                   ENDIF
051800000801     C*
051900000801     C                   ENDSR
052000000801     C***
052100000801
052200011113
052300011113     C*----------------------------------------------------*
052400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
052500011113     C*----------------------------------------------------*
052600011113     C     CHKIMPDIV     BEGSR
052700011113     C*
052800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
052900011113     C                   Z-ADD     *zeros        wrkDec            9 9
053000011113     C*
053100011113     C* Come prima cosa effettuo considerazioni sulla divisa
053200011113     C                   IF        vabIAS > *zeros
053300011113     C                   IF        vabVAS <> 'EUR'
053400011113     C                   EVAL      vabVAS =  'ITL'
053500011113     C                   ENDIF
053600011113     C                   ENDIF
053700011113     C*
053800011113     C                   IF        vabCAS > *zeros
053900011113     C                   IF        vabVCA <> 'EUR'
054000011113     C                   EVAL      vabVCA =  'ITL'
054100011113     C                   ENDIF
054200011113     C                   ENDIF
054300011113     C*
054400011113     C                   IF        vabVMD > *zeros
054500020305     C                   IF        vabVAD <> 'EUR'
054600011113     C                   EVAL      vabVAD =  'ITL'
054700011113     C                   ENDIF
054800011113     C                   ENDIF
054900011113     C*
055000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
055100011113     C                   Z-ADD     vabIAS        wrkDec
055200011113     C                   IF        wrkDec > *zeros
055300011113     C                   IF        vabVAS = 'ITL'
055400011113     C                   EVAL      vabIAS = *zeros
055500011113     C                   ENDIF
055600011113     C                   ENDIF
055700011113     C*
055800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
055900011113     C                   Z-ADD     vabCAS        wrkDec
056000011113     C                   IF        wrkDec > *zeros
056100011113     C                   IF        vabVCA = 'ITL'
056200011113     C                   EVAL      vabCAS = *zeros
056300011113     C                   ENDIF
056400011113     C                   ENDIF
056500011113     C*
056600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
056700011113     C                   Z-ADD     vabVMD        wrkDec
056800011113     C                   IF        wrkDec > *zeros
056900011113     C                   IF        vabVAD = 'ITL'
057000011113     C                   EVAL      vabVMD = *zeros
057100011113     C                   ENDIF
057200011113     C                   ENDIF
057300011113     C*
057400011113     C                   ENDSR
057500011113     C***
057600011113
057700011113
057800000801
057900000801
058000990920      /TITLE Invio dei dati al punto operativo.
058100010202     C     invio         BEGSR
058200990920     C*
058300080512     C* 1° invio EDIVAT
058400010201     C                   reset                   dscmz
058500010201     C                   move      vlrpoi        cmzdst
058600080512     C                   eval      cmzfld = 'EDIVATWR'
058700010201     C                   eval      cmzmbd = vlrhdl
058800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
058900021009     C***                if        prmfir = *blanks
059000080512     C                   eval      cmzfla = 'EDIVAT0F'
059100080512     C                   eval      cmzmba = 'EDIVAT0F'
059200021009     C***                else
059300021009     C***                eval      cmzfla = prmfir
059400021009     C***                eval      cmzmba = prmfir
059500021009     C***                endif
059600010201     C                   eval      cmznrr = *zeros
059700020305     C                   move      §ctrokvt      cmznrr
059800021018     C                   eval      cmzlba = vlrfl1
059900010201     C                   call(e)   'TIS711C'
060000010201     C                   parm                    dscmz
060100010201     C                   parm      *blanks       esito
060200010205     C                   if        %error
060300010205     C                             or cmzerr = '1'
060400010205     C                             or esito  = '1'
060500010205     C                   eval      wrkesito = '3'
060600010205     C                   else
060700010201     C*
060800080512     C* 2° invio EDIVAB
060900010201     C                   reset                   dscmz
061000010201     C                   move      vlrpoi        cmzdst
061100010201     C                   eval      cmzfld = vlrfou
061200010201     C                   eval      cmzmbd = vlrhdl
061300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
061400021009     C***                if        prmfir = *blanks
061500080512     C                   eval      cmzfla = 'EDIVAB0F'
061600080512     C                   eval      cmzmba = 'EDIVAB0F'
061700021009     C***                else
061800021009     C***                eval      cmzfla = prmfir
061900021009     C***                eval      cmzmba = prmfir
062000021009     C***                endif
062100010201     C                   eval      cmznrr = *zeros
062200010201     C                   move      §ctrokvb      cmznrr
062300021018     C                   eval      cmzlba = vlrfl1
062400010201     C                   call(e)   'TIS711C'
062500010201     C                   parm                    dscmz
062600010201     C                   parm      *blanks       esito
062700010201     C                   if        %error
062800010201     C                             or cmzerr = '1'
062900010201     C                             or esito  = '1'
063000010201     C                   eval      wrkesito = '3'
063100010201     C                   endif
063200010205     C                   endif
063300990920     C*
063400000613     C                   ENDSR
063500000613     C***
063600070411
063700070411     C     *pssr         BEGSR
063800070411     C*
063900070411     C                   if        %open(tivin00r)
064000070411     C                   close     tivin00r
064100070411     C                   endif
064200080512     C                   if        %open(edivabwr)
064300080512     C                   close     edivabwr
064400070411     C                   endif
064500080512     C                   if        %open(edivatwr)
064600080512     C                   close     edivatwr
064700070411     C                   endif
064800070411     C*
064900070411     C* Effettuo la chiamata al CLLE preposto
065000080512     C                   call(e)   'TITVEVTC'
065100070411     C                   parm                    parccm
065200070411     C                   parm                    parmbr
065300070411     C                   parm      '2'           paropz
065400070411     C*
065500070411     C                   eval      wrkesito = '2'
065600070411     C*
065700070411     C                   seton                                        LR
065800070411     C*
065900070411     C                   ENDSR     '*CANCL'
066000070411     C***
066100070411
066200990910
066300000613     C     *inzsr        BEGSR
066400990910     C*
066500990910     C     *entry        plist
066600990920     C                   parm                    tivlrds
066700990921     C                   parm      wrkesito      esito
066800000724     C                   parm                    prmlit
066900000710     C                   parm                    prmfir
067000080512     C*
067100080512     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
067200080512     C                   move(p)   vlrMSG        wNomeFile
067300000613     C*
067400000830     C* CALCOLA LA DATA CORRENTE
067500000830     C                   time                    wn14             14 0
067600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
067700080512     C                   movel     wn14          oracor            6 0          *ORA
067800000830     C                   z-add     wn8           g08dat
067900000830     C                   z-add     *zeros        g08inv
068000000830     C                   movel     '0'           g08err
068100000830     C                   call      'XSRDA8'
068200000830     C                   parm                    wlbda8
068300000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
068400000830     C*
068500000613     C                   ENDSR
068600000613     C***
068700990908
068800080512     Otitv1g2p  E            riepilogo         2
068900990915     O                                              'Upload via Internet'
069000990915     O                                           +1 'Traduzione TIVIN00R -'
069100080512     O                                           +1 'EDIVABWR/EDIVATWR'
069200010201     O                                           +1 'Report testate bolle'
069300990915     O          E            riepilogo   2
069400990915     O                       wrkdata
069500990915     O                       wrkora              +1
069600990915     O                       procname            +1
069700990915     O          E            riepilogo   2
069800990915     O                                              'Cliente..................:'
069900990915     O                       VABCCM        z     +1
070000990915     O          E            riepilogo   2
070100990920     O                                              'Riferimento Strategi.....:'
070200990920     O                       vlrhdl              +1
070300990915     O          E            riepilogo   2
070400990915     O                                              'Giusti...................:'
070500010201     O                       §CTROKVB      2   +  1
070600990915     O          E            riepilogo   2
070700990915     O                                              'Sbagliati e corretti.....:'
070800971022     O                       §CTRMO        2   +  1
070900990915     O          E            riepilogo   2
071000990915     O                                              'Sbagliati e scartati.....:'
071100971022     O                       §CTRNO        2   +  1
071200000613
071300080512     Otitv1g2ps E            testdett          2
071400000613     O                                              'Upload via Internet'
071500000613     O                                           +1 'Traduzione TIVIN00R -'
071600080512     O                                           +1 'EDIVABWR/EDIVATWR'
071700010201     O                                           +1 'Report testate bolle'
071800000616     O          E            testdett    3
071900000613     O                                           +2 'N° rec'
072000000613     O                                           +3 'Anteprima contenuto'
072100000616     O          E            rigadett    2
072200000613     O                       rrnum               +2
072300000621     O                       recko               +3
072400000616     O          E            findett     2
072500000613     O                       wrkdata
072600000613     O                       wrkora              +1
072700000613     O                       procname            +1
072800000616     O          E            findett     2
072900000613     O                                              'Cliente..................:'
073000000613     O                       VABCCM        z     +1
073100000616     O          E            findett     2
073200000613     O                                              'Riferimento Strategi.....:'
073300000613     O                       vlrhdl              +1
073400000616     O          E            findett     2
073500000613     O                                              'Giusti...................:'
073600010201     O                       §CTROKVB      2   +  1
073700000616     O          E            findett     2
073800000613     O                                              'Sbagliati e corretti.....:'
073900000613     O                       §CTRMO        2   +  1
074000000616     O          E            findett     2
074100000613     O                                              'Sbagliati e scartati.....:'
074200000613     O                       §CTRNO        2   +  1
074300000512** CMD - COMANDI CL
074400080512OVRPRTF FILE(TITV1G2P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074500080512OVRPRTF FILE(TITV1G2PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074600080512DLTOVR FILE(TITV1G2P TITV1G2PS) LVL(*)
074700000512
074800000512
