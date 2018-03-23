000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800100722     Ftitv1z0p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100100722     Ftitv1z0ps O    f  198        PRINTER usropn
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
002600070719     D tisi95ds      e ds
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
004000000830
004100041025     D*------------------
004200041025     D* DS REPERIMENTO NUMERATORE
004300041025     D*------------------
004400041025     D trul33ds      e ds                  inz
004500041025     D*------------------
004600041025     D* DS ARCHITETTURA
004700041025     D*------------------
004800041025     D kpjba         e ds                  inz
004900041025     D*------------------
005000990908
005100010201
005200010201
005300990915     C                   time                    wrkdata
005400990915     C                   time                    wrkora
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800000724     C*
005900000724     C* SE OCCORRE SPEDIRE IN FILIALE
006000000724     C                   if        vlrpoi <> *zeros
006100000724     C*
006200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006400000724     C     vlrpoi        chain     azorg01l
006500000724     C                   if        %found
006600000616     C                   movel(p)  CMD(1)        depcmd
006700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006800000616     C*
006900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
007000011113     C                   Z-ADD     150           LENGH            15 5
007100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007200000616     C                   PARM                    depcmd
007300000616     C                   PARM                    LENGH
007400000724     C*
007500000724     C                   endif
007600000724     C                   endif
007700000616     C*
007800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007900000616     C                   movel(p)  CMD(2)        depcmd
008000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
008100000616     C*
008200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008300011113     C                   Z-ADD     150           LENGH            15 5
008400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008500000616     C                   PARM                    depcmd
008600000616     C                   PARM                    LENGH
008700000616     C*
008800100722     C                   if        not %open(titv1z0ps)
008900100722     C                   open      titv1z0ps
009000000616     C                   except    testdett
009100000613     C                   endif
009200000613     C*
009300040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
009500070719     C*
009600070719     C* Esegue lancio TISI95R solo x chiusura
009700070719     C                   CLEAR                   TISI95DS
009800070719     C                   EVAL      I95TLA = 'C'
009900070719     C                   CALL      'TISI95R'
010000070719     C                   PARM                    TISI95DS
010100000613     C*
010200010202     C* Effettuo la chiamata al CLLE preposto
010300040506     C                   call(e)   'TITVVTC'
010400010202     C                   parm                    parccm
010500010202     C                   parm                    parmbr
010600010202     C                   parm      '2'           paropz
010700010202     C*
010800100722     C                   if        %open(titv1z0ps)
010900000616     C                   except    findett
011000100722     C                   close     titv1z0ps
011100000613     C                   endif
011200000616     C*
011300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000616     C                   PARM                    CMD(3)
011700000616     C                   PARM                    LENGH
011800000616     C*
011900010201     C                   seton                                        LR
012000000613
012100000613
012200000613     C*--------------------------------------------------------
012300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012400000613     C*--------------------------------------------------------
012500000621     C     STPR          BEGSR
012600000613     C*
012700100722     C                   if        not %open(titv1z0p)
012800100722     C                   open      titv1z0p
012900990915     C                   endif
013000990915     C*
013100990915     C                   except    riepilogo
013200990915     C*
013300100722     C                   if        %open(titv1z0p)
013400100722     C                   close     titv1z0p
013500990914     C                   endif
013600990910     C*
013700000613     C                   ENDSR
013800000613     C***
013900990908
014000000801
014100910830     C*--------------------------------------------------------
014200040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014300910830     C*--------------------------------------------------------
014400040526     C     RWFILE        BEGSR
014500990910     C*
014600990914     C                   if        not %open(tivin00r)
014700990908     C                   open      tivin00r
014800990914     C                   endif
014900021113     C                   if        not %open(fivabwwr)
015000021113     C                   open      fivabwwr
015100990914     C                   endif
015200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015300020305     C                   exsr      prevat
015400010201     C*
015500010202     C                   if        chkcall = '0'
015600010202     C*
015700021113     C                   if        not %open(fivatwwr)
015800021113     C                   open      fivatwwr
015900010201     C                   endif
016000990910     C*
016100010201     C                   clear                   §CTROKVB          5 0
016200020305     C                   clear                   §CTROKVT          5 0
016300000801     C                   clear                   §CTRMO            5 0
016400000801     C                   clear                   §CTRNO            5 0
016500100729     C*
016600100729     C                   z-add     1             wGiro             1 0
016700040910     C*
016800921023     C                   DO        *HIVAL
016900990913     C*
017000990915     C                   READ      tivin00r                               70
017100040910     C                   if        vindta > *blanks
017200000613     C                   add       1             rrnum
017300000801     C*
017400000801     C                   if        *in70 = *off
017500000801     C                             and
017600000801     C                             (vinflg = *blanks
017700000801     C                              or vinflg = '0'
017800000801     C                              or vinflg = '2')
017900000801     C*
018000000801     C                   clear                   vinmsg
018100000801     C                   eval      vinflg = '1'
018200040910     C*
018300040910     C* Eseguo routine d traduzione
018400040910     C                   exsr      impvabvat
018500040802     C*
018600010305     C                   endif
018700000905     C*
018800000905     C                   else
018900000905     C                   eval      vinflg = '1'
019000000905     C                   endif
019100000905     C*
019200000905     C  N70              update    tivin000
019300000905     C*
019400991022     C  N70              ENDdo
019500100722     C*
019600100722     C* Scarico i buffer testata ancora "in canna"
019700100722     C                   WRITE     FIVAB000
019800010202     C*
019900010202     C                   endif
020000990910
020100990910     C* Se non ci sono record con errori ...
020200000710     C                   if        §ctrno = 0
020300990910     C* ... restituisco esito OK.
020400990921     C                   eval      wrkesito = '0'
020500990910     C                   else
020600010201     C                   if        §ctrokvb > 0
020700990921     C                   eval      wrkesito = '1'
020800000710     C                   else
020900000710     C                   eval      wrkesito = '2'
021000990910     C                   endif
021100000710     C                   endif
021200990910     C*
021300990914     C                   if        %open(tivin00r)
021400990908     C                   close     tivin00r
021500990914     C                   endif
021600021113     C                   if        %open(fivabwwr)
021700021113     C                   close     fivabwwr
021800990914     C                   endif
021900021113     C                   if        %open(fivatwwr)
022000021113     C                   close     fivatwwr
022100010201     C                   endif
022200990910     C*
022300010201     C                   if        §ctrokvb > 0
022400000724     C                             and vlrpoi <> *zeros
022500010202     C                   exsr      invio
022600990920     C                   endif
022700990920     C*
022800910830     C                   ENDSR
022900000613     C***
023000990920
023100000801     C*----------------------------------------------------*
023200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023300000801     C*----------------------------------------------------*
023400010201     C     INZVAR        BEGSR
023500000801     C*
023600040802     C                   Z-ADD     *zeros        Num5_0            5 0
023700040802     C                   MOVEL     '0'           FlgCAS            1
023800000801     C*
023900000801     C                   ENDSR
024000000801     C*----------------------------------------------------*
024100040910     C*  IMPOSTAZIONE CAMPI COSTANTI
024200000801     C*----------------------------------------------------*
024300000801     C     DEFCAM        BEGSR
024400000801     C*
024500021113     C                   CLEAR                   FIVAB000
024600040802     C                   CLEAR                   FIVAT000
024700020619     C* Imposto i valori di default...
024800100722     C***                Z-ADD     0895741       VABCCM
024900100722     C***                Z-ADD     0895741       VATCCM
025000100714     C                   Z-ADD     089           VABLNP
025100100714     C                   Z-ADD     089           VATLNP
025200070531     C                   Z-ADD     000           VABCTR
025300070531     C                   MOVEL     '7Q'          VABCTM
025400040823     C                   MOVEL     '1'           VABCBO
025500020619     C* ... e poi verifico se sono stati passati come parametri
025600020619     C                   IF        vlrppt > *blanks
025700040506     C                   IF        %subst(vlrppt:1:7) <> *blanks
025800020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025900020619     C                   EXSR      CHKNUM
026000020619     C                   IF        PiInt=*on
026100020619     C                   Z-ADD     PiVal         VABCCM
026200020619     C                   Z-ADD     PiVal         VATCCM
026300020619     C                   ENDIF
026400040506     C                   ENDIF
026500040506     C                   IF        %subst(vlrppt:8:3) <> *blanks
026600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026700020619     C                   EXSR      CHKNUM
026800020619     C                   IF        PiInt=*on
026900020619     C                   Z-ADD     PiVal         VABLNP
027000020619     C                   Z-ADD     PiVal         VATLNP
027100040506     C                   ENDIF
027200020619     C                   ENDIF
027300040506     C                   IF        %subst(vlrppt:11:3) <> *blanks
027400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027500020619     C                   EXSR      CHKNUM
027600020619     C                   IF        PiInt=*on
027700020619     C                   Z-ADD     PiVal         VABCTR
027800040506     C                   ENDIF
027900020619     C                   ENDIF
028000060202     C                   IF        %subst(vlrppt:14:2) <> *blanks
028100060202     C                   EVAL      VABCTM=%trim(%subst(vlrppt:14:2))
028200060202     C                   ENDIF
028300020619     C                   ENDIF
028400000801     C*
028500000801     C                   ENDSR
028600000801     C*----------------------------------------------------*
028700040910     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB e FIVAT)
028800000801     C*----------------------------------------------------*
028900040910     C     IMPVABVAT     BEGSR
029000040910     C*
029100040910     C* Traduzione relativa ai tipi record del file del cliente
029200040910     C*
029300071210     C*
029400071210     C***
029500071210     C* ...tipo record 'EBE' (testata)
029600071210     C                   IF        %subst(vindta:1:3) = 'EBE'
029700100722     C*
029800100722     C* Se nn primo giro => scarico il buffer precedente
029900100722     C                   if        wGiro = 1
030000100722     C                   eval      wGiro = 2
030100100722     C                   else
030200100722     C                   WRITE     FIVAB000
030300100722     C                   endif
030400100722     C*
030500071210     C* Resetto indicatore d anomalia sul singolo record
030600071210     C                   eval      vinflg = '1'
030700071210     C* ......inizializzazioni iniziali e formati record file Bartolini
030800071210     C                   EXSR      INZVAR
030900071210     C                   EXSR      DEFCAM
031000071210     C*
031100071210     C                   Z-ADD     *zeros        errore            1 0
031200071210     C* ......valorizzazione campi da data traduzione e da P.O. d gestione
031300071210     C                   MOVEL     datcor        VABAAS
031400071210     C                   MOVEL     datcor        VATAAS
031500071210     C                   MOVE      datcor        VABMGS
031600071210     C                   MOVE(P)   vlrpoi        VABFGS
031700071210     C                   MOVE(P)   vlrpoi        VATFGS
031800071210     C* ......VABNSP/VATNSP
031900071210     C* NSP => Stacco un numeratore da AZNUM
032000071210     C                   clear                   TRUL33DS
032100071210     C                   eval      I33OPE = *zeros
032200071210     C                   eval      I33CNU = 302
032300071210     C                   eval      I33NUM = 1
032400071210     C                   movel     TRUL33DS      KPJBU
032500071210     C                   call      'TRUL33R'
032600071210     C                   parm                    KPJBA
032700071210     C                   movel     KPJBU         TRUL33DS
032800071210     C                   if        O33ERR = *zeros
032900071210     C                   z-add     O33NRF        VABNSP
033000071210     C                   z-add     O33NRF        VATNSP
033100071210     C                   else
033200071210     C                   Z-ADD     1             errore
033300071210     C                   EVAL      vinmsg = %trimr(vinmsg)
033400071210     C                             + ' ' + 'VABNSP VATNSP'
033500071210     C                   endif
033600080211     C* ......VABNT2
033700080211     C                   EVAL      VABNT2='CUST ACCOUNT #: ' +
033800080211     C                                    %trim(%subst(vindta:9:13))
033900060202     C* ......VABRMN
034000071210     C                   EVAL      PiStr=%trim(%subst(vindta:22:15))
034100060202     C                   EXSR      CHKNUM
034200060202     C                   IF        PiInt=*on
034300060202     C                   Z-ADD     PiVal         VABRMN
034400060202     C                   ELSE
034500060202     C                   ADD       1             errore
034600060202     C                   EVAL      vinmsg = %trimr(vinmsg)
034700060202     C                             + ' ' + 'VABRMN'
034800060202     C                   ENDIF
034900100714     C* ......VABNCL
035000100714     C                   EVAL      PiStr=%trim(%subst(vindta:275:5))
035100100714     C                   EXSR      CHKNUM
035200100714     C                   IF        PiInt=*on
035300100714     C                   Z-ADD     PiVal         VABNCL
035400100714     C                   ELSE
035500100714     C                   ADD       1             errore
035600100714     C                   EVAL      vinmsg = %trimr(vinmsg)
035700100714     C                             + ' ' + 'VABNCL'
035800100714     C                   ENDIF
035900060202     C* ......VABPKB
036000100714     C                   EVAL      PiStr=%trim(%subst(vindta:280:9))
036100060202     C                   EXSR      CHKNUM
036200060202     C                   IF        PiNum=*on
036300070726     C                   EVAL      PiVal = PiVal / 1000                         da grammi a kg
036400070726     C                   Z-ADD(H)  PiVal         VABPKB
036500060202     C                   ELSE
036600060202     C                   ADD       1             errore
036700060202     C                   EVAL      vinmsg = %trimr(vinmsg)
036800060202     C                             + ' ' + 'VABPKB'
036900060202     C                   ENDIF
037000071210     C* ......VABVLB
037100100722     C                   EVAL      PiStr=%trim(%subst(vindta:289:9))
037200100722     C                   EXSR      CHKNUM
037300100722     C                   IF        PiNum=*on
037400100722     C                   EVAL      PiVal = PiVal / 1000                         da dm3 a m3
037500100722     C                   Z-ADD(H)  PiVal         VABVLB
037600100722     C                   ELSE
037700100722     C                   ADD       1             errore
037800100722     C                   EVAL      vinmsg = %trimr(vinmsg)
037900100722     C                             + ' ' + 'VABVLB'
038000100722     C                   ENDIF
038100071210     C* ......VABRMA
038200100722     C                   EVAL      VABRMA=%trim(%subst(vindta:353:35))
038300060202     C* ......VABRSD
038400071210     C                   EVAL      VABRSD=%trim(%subst(vindta:53:35))
038500100714     C* ......VABIND
038600100722     C                   EVAL      VABIND=%trim(%subst(vindta:123:35))
038700060202     C* ......VABLOD
038800070719     C                   EVAL      VABLOD=%trim(%subst(vindta:237:35))
038900060202     C* ......VABCAD
039000070719     C                   EVAL      PiStr=%trim(%subst(vindta:228:9))
039100060202     C                   EXSR      CHKNUM
039200060202     C                   IF        PiInt=*on
039300060202     C                   Z-ADD     PiVal         Num5_0
039400060202     C                   MOVEL(P)  Num5_0        VABCAD
039500060202     C                   ELSE
039600060202     C                   ADD       1             errore
039700060202     C                   EVAL      vinmsg = %trimr(vinmsg)
039800060202     C                             + ' ' + 'VABCAD'
039900060202     C                   ENDIF
040000060202     C* ......VABPRD
040100100722     C                   EVAL      VABPRD=%trim(%subst(vindta:160:2))
040200070719     C* Reperisco la provincia dal CAP e dalla località
040300070719     C                   IF        VABPRD  = *blanks AND
040400070719     C                             VABCAD <> *blanks
040500070719     C                   CLEAR                   TISI95DS
040600070719     C                   EVAL      I95TCN = '3'
040700070719     C                   Z-ADD     datcor        I95DAT
040800070719     C                   EVAL      I95CAP = VABCAD
040900070719     C                   EVAL      I95LOC = VABLOD
041000070719     C                   CALL      'TISI95R'
041100070719     C                   PARM                    TISI95DS
041200070719     C                   EVAL      VABPRD = O95PRV
041300070719     C                   ENDIF
041400100722     C* ......VABDCR
041500100722     C                   EVAL      PiStr=%trim(%subst(vindta:45:8))
041600100722     C                   EXSR      CHKNUM
041700100722     C                   IF        PiInt=*on
041800100722     C                   Z-ADD     PiVal         VABDCR
041900100722     C                   ELSE
042000100722     C                   ADD       1             errore
042100100722     C                   EVAL      vinmsg = %trimr(vinmsg)
042200100722     C                             + ' ' + 'VABDCR'
042300100722     C                   ENDIF
042400071210     C*
042500060202     C*
042600060202     C* Considerazioni sul contenuto di campi precedentemente valorizzati
042700060202     C                   IF        FlgCAS <> '0'
042800060202     C                   IF        VABCBO = '1'
042900060202     C                   EVAL      VABCBO = '4'
043000060202     C                   ELSE
043100060202     C                   EVAL      VABCBO = '6'
043200060202     C                   ENDIF
043300060202     C                   ENDIF
043400060202     C*
043500060202     C* Eseguo routine finale x considerazioni specifiche su importi/divise
043600060202     C                   EXSR      CHKIMPDIV
043700040910     C*
043800040910     C                   ENDIF
043900040910     C***
044000070719     C* ...tipo record 'EBC' (dettaglio barcode segnacooli)
044100070719     C                   IF        %subst(vindta:1:3) = 'EBC'
044200070719     C* Resetto indicatore d anomalia sul singolo record
044300070719     C                   eval      vinflg = '1'
044400100722     C* ......VABCCM
044500100722     C                   EVAL      PiStr=%trim(%subst(vindta:57:6))
044600100722     C                   EXSR      CHKNUM
044700100722     C                   IF        PiInt=*on
044800100722     C                   Z-ADD     PiVal         VABCCM
044900100722     C                   Z-ADD     PiVal         VATCCM
045000100722     C                   ELSE
045100100722     C                   ADD       1             errore
045200100722     C                   EVAL      vinmsg = %trimr(vinmsg)
045300100722     C                             + ' ' + 'VABCCM VATCCM'
045400100722     C                   ENDIF
045500100722     C* Controlli e "particolarità" x codice cliente
045600100722     C                   select
045700100722     C                   when      VABCCM = 0895741
045800100722     C                   z-add     *zeros        VABDCR
045900100722     C                   when      VABCCM = 0895742
046000100722     C                   when      VABCCM = 0895743
046100100722     C                   z-add     *zeros        VABDCR
046200100722     C                   other
046300100722     C                   Z-ADD     0895741       VABCCM
046400100722     C                   Z-ADD     0895741       VATCCM
046500100722     C                   z-add     *zeros        VABDCR
046600100722     C                   endsl
046700100722     C* ......VATNOT
046800100722     C                   EVAL      VATNOT=%trim(%subst(vindta:45:30))           * sarebbe 32 char!!
046900100722     C                   EVAL      VATTRC = 'E'
047000100722     C* x ogni tipo record 'EBC' devo scaricare il buffer del file FIVAT
047100100722     C                   IF        VATNOT <> *blanks
047200100722     C                   ADD       1             §CTROKVT
047300100722     C                   WRITE     FIVAT000
047400100722     C                   ENDIF
047500100722     C*
047600100722     C                   ENDIF
047700010202     C*
047800000801     C* Ebbene...
047900000801     C                   ADD       1             §CTRMO
048000010201     C                   IF        errore <> *zeros
048100000801     C                   ADD       1             §CTRNO
048200000801     C                   EVAL      recko = vindta
048300000801     C                   EXCEPT    rigadett
048400000801     C                   EVAL      vinflg = '2'
048500000801     C                   ELSE
048600010201     C                   ADD       1             §CTROKVB
048700000801     C                   ENDIF
048800000801     C*
048900000801     C                   ENDSR
049000010202     C*----------------------------------------------------*
049100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049200010202     C*----------------------------------------------------*
049300020305     C     PREVAT        BEGSR
049400010202     C*
049500021113     C* Compongo il nome del membro da dare al FIVATWWR
049600010202     C                   eval      parmbr = vlrhdl
049700010202     C                   movel     'M'           parmbr
049800060113     C                   eval      parccm = vlrksc
049900010202     C                   eval      paropz = '1'
050000010202     C* Effettuo la chiamata al CLLE preposto
050100040506     C                   call(e)   'TITVVTC'
050200010202     C                   parm                    parccm
050300010202     C                   parm                    parmbr
050400010202     C                   parm                    paropz
050500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050600010202     C                   if        %error
050700010202     C                   movel     '1'           chkcall
050800010202     C                   else
050900010202     C                   movel     '0'           chkcall
051000010202     C                   endif
051100010202     C*
051200010202     C                   ENDSR
051300000801     C*----------------------------------------------------*
051400000801     C*  CONTROLLO NUMERICITA' CAMPI
051500000801     C*----------------------------------------------------*
051600000801     C     CHKNUM        BEGSR
051700000801     C*
051800000801     C                   call(e)   'ISNUMERIC'
051900000801     C                   PARM                    PiStr            30
052000040714     C                   PARM      ','           PiDecChr          1
052100000801     C                   PARM      *ZEROS        PiVal            30 9
052200000801     C                   PARM      '0'           PiInt             1
052300000801     C                   PARM      '0'           PiNum             1
052400000801     C                   IF        %error
052500000801     C                   EVAL      PiInt=*off
052600000801     C                   ENDIF
052700000801     C*
052800000801     C                   ENDSR
052900000801     C***
053000000801
053100011113
053200011113     C*----------------------------------------------------*
053300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
053400011113     C*----------------------------------------------------*
053500011113     C     CHKIMPDIV     BEGSR
053600011113     C*
053700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
053800011113     C                   Z-ADD     *zeros        wrkDec            9 9
053900011113     C*
054000011113     C* Come prima cosa effettuo considerazioni sulla divisa
054100011113     C                   IF        vabIAS > *zeros
054200011113     C                   IF        vabVAS <> 'EUR'
054300011113     C                   EVAL      vabVAS =  'ITL'
054400011113     C                   ENDIF
054500011113     C                   ENDIF
054600011113     C*
054700011113     C                   IF        vabCAS > *zeros
054800011113     C                   IF        vabVCA <> 'EUR'
054900011113     C                   EVAL      vabVCA =  'ITL'
055000011113     C                   ENDIF
055100011113     C                   ENDIF
055200011113     C*
055300011113     C                   IF        vabVMD > *zeros
055400020305     C                   IF        vabVAD <> 'EUR'
055500011113     C                   EVAL      vabVAD =  'ITL'
055600011113     C                   ENDIF
055700011113     C                   ENDIF
055800011113     C*
055900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
056000011113     C                   Z-ADD     vabIAS        wrkDec
056100011113     C                   IF        wrkDec > *zeros
056200011113     C                   IF        vabVAS = 'ITL'
056300011113     C                   EVAL      vabIAS = *zeros
056400011113     C                   ENDIF
056500011113     C                   ENDIF
056600011113     C*
056700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
056800011113     C                   Z-ADD     vabCAS        wrkDec
056900011113     C                   IF        wrkDec > *zeros
057000011113     C                   IF        vabVCA = 'ITL'
057100011113     C                   EVAL      vabCAS = *zeros
057200011113     C                   ENDIF
057300011113     C                   ENDIF
057400011113     C*
057500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
057600011113     C                   Z-ADD     vabVMD        wrkDec
057700011113     C                   IF        wrkDec > *zeros
057800011113     C                   IF        vabVAD = 'ITL'
057900011113     C                   EVAL      vabVMD = *zeros
058000011113     C                   ENDIF
058100011113     C                   ENDIF
058200011113     C*
058300011113     C                   ENDSR
058400011113     C***
058500011113
058600011113
058700000801
058800000801
058900990920      /TITLE Invio dei dati al punto operativo.
059000010202     C     invio         BEGSR
059100990920     C*
059200021113     C* 1° invio FIVAT
059300010201     C                   reset                   dscmz
059400010201     C                   move      vlrpoi        cmzdst
059500021113     C                   eval      cmzfld = 'FIVATWWR'
059600010201     C                   eval      cmzmbd = vlrhdl
059700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
059800021009     C***                if        prmfir = *blanks
059900021113     C                   eval      cmzfla = 'FIVAT00F'
060000021113     C                   eval      cmzmba = 'FIVAT00F'
060100021009     C***                else
060200021009     C***                eval      cmzfla = prmfir
060300021009     C***                eval      cmzmba = prmfir
060400021009     C***                endif
060500010201     C                   eval      cmznrr = *zeros
060600020305     C                   move      §ctrokvt      cmznrr
060700021018     C                   eval      cmzlba = vlrfl1
060800010201     C                   call(e)   'TIS711C'
060900010201     C                   parm                    dscmz
061000010201     C                   parm      *blanks       esito
061100010205     C                   if        %error
061200010205     C                             or cmzerr = '1'
061300010205     C                             or esito  = '1'
061400010205     C                   eval      wrkesito = '3'
061500010205     C                   else
061600010201     C*
061700021113     C* 2° invio FIVAB
061800010201     C                   reset                   dscmz
061900010201     C                   move      vlrpoi        cmzdst
062000010201     C                   eval      cmzfld = vlrfou
062100010201     C                   eval      cmzmbd = vlrhdl
062200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
062300021009     C***                if        prmfir = *blanks
062400021113     C                   eval      cmzfla = 'FIVAB00F'
062500021113     C                   eval      cmzmba = 'FIVAB00F'
062600021009     C***                else
062700021009     C***                eval      cmzfla = prmfir
062800021009     C***                eval      cmzmba = prmfir
062900021009     C***                endif
063000010201     C                   eval      cmznrr = *zeros
063100010201     C                   move      §ctrokvb      cmznrr
063200021018     C                   eval      cmzlba = vlrfl1
063300010201     C                   call(e)   'TIS711C'
063400010201     C                   parm                    dscmz
063500010201     C                   parm      *blanks       esito
063600010201     C                   if        %error
063700010201     C                             or cmzerr = '1'
063800010201     C                             or esito  = '1'
063900010201     C                   eval      wrkesito = '3'
064000010201     C                   endif
064100010205     C                   endif
064200990920     C*
064300000613     C                   ENDSR
064400000613     C***
064500070411
064600070411     C     *pssr         BEGSR
064700070411     C*
064800070411     C                   if        %open(tivin00r)
064900070411     C                   close     tivin00r
065000070411     C                   endif
065100070411     C                   if        %open(fivabwwr)
065200070411     C                   close     fivabwwr
065300070411     C                   endif
065400070411     C                   if        %open(fivatwwr)
065500070411     C                   close     fivatwwr
065600070411     C                   endif
065700070411     C*
065800070411     C* Effettuo la chiamata al CLLE preposto
065900070411     C                   call(e)   'TITVVTC'
066000070411     C                   parm                    parccm
066100070411     C                   parm                    parmbr
066200070411     C                   parm      '2'           paropz
066300070411     C*
066400070411     C                   eval      wrkesito = '2'
066500070411     C*
066600070411     C                   seton                                        LR
066700070411     C*
066800070411     C                   ENDSR     '*CANCL'
066900070411     C***
067000070411
067100990910
067200000613     C     *inzsr        BEGSR
067300990910     C*
067400990910     C     *entry        plist
067500990920     C                   parm                    tivlrds
067600990921     C                   parm      wrkesito      esito
067700000724     C                   parm                    prmlit
067800000710     C                   parm                    prmfir
067900000613     C*
068000000830     C* CALCOLA LA DATA CORRENTE
068100100722     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
068200100722     C                   eval      datcor = %dec(%date() : *ISO)
068300000830     C*
068400000613     C                   ENDSR
068500000613     C***
068600990908
068700100722     Otitv1z0p  E            riepilogo         2
068800990915     O                                              'Upload via Internet'
068900990915     O                                           +1 'Traduzione TIVIN00R -'
069000021113     O                                           +1 'FIVABWWR/FIVATWWR'
069100010201     O                                           +1 'Report testate bolle'
069200990915     O          E            riepilogo   2
069300990915     O                       wrkdata
069400990915     O                       wrkora              +1
069500990915     O                       procname            +1
069600990915     O          E            riepilogo   2
069700990915     O                                              'Cliente..................:'
069800990915     O                       VABCCM        z     +1
069900990915     O          E            riepilogo   2
070000990920     O                                              'Riferimento Strategi.....:'
070100990920     O                       vlrhdl              +1
070200990915     O          E            riepilogo   2
070300990915     O                                              'Giusti...................:'
070400010201     O                       §CTROKVB      2   +  1
070500990915     O          E            riepilogo   2
070600990915     O                                              'Sbagliati e corretti.....:'
070700971022     O                       §CTRMO        2   +  1
070800990915     O          E            riepilogo   2
070900990915     O                                              'Sbagliati e scartati.....:'
071000971022     O                       §CTRNO        2   +  1
071100000613
071200100722     Otitv1z0ps E            testdett          2
071300000613     O                                              'Upload via Internet'
071400000613     O                                           +1 'Traduzione TIVIN00R -'
071500021113     O                                           +1 'FIVABWWR/FIVATWWR'
071600010201     O                                           +1 'Report testate bolle'
071700000616     O          E            testdett    3
071800000613     O                                           +2 'N° rec'
071900000613     O                                           +3 'Anteprima contenuto'
072000000616     O          E            rigadett    2
072100000613     O                       rrnum               +2
072200000621     O                       recko               +3
072300000616     O          E            findett     2
072400000613     O                       wrkdata
072500000613     O                       wrkora              +1
072600000613     O                       procname            +1
072700000616     O          E            findett     2
072800000613     O                                              'Cliente..................:'
072900000613     O                       VABCCM        z     +1
073000000616     O          E            findett     2
073100000613     O                                              'Riferimento Strategi.....:'
073200000613     O                       vlrhdl              +1
073300000616     O          E            findett     2
073400000613     O                                              'Giusti...................:'
073500010201     O                       §CTROKVB      2   +  1
073600000616     O          E            findett     2
073700000613     O                                              'Sbagliati e corretti.....:'
073800000613     O                       §CTRMO        2   +  1
073900000616     O          E            findett     2
074000000613     O                                              'Sbagliati e scartati.....:'
074100000613     O                       §CTRNO        2   +  1
074200000512** CMD - COMANDI CL
074300100722OVRPRTF FILE(TITV1Z0P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074400100722OVRPRTF FILE(TITV1Z0PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
074500100722DLTOVR FILE(TITV1Z0P TITV1Z0PS) LVL(*)
074600000512
074700000512
