000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800071030     Ftitv1a7p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100071030     Ftitv1a7ps O    f  198        PRINTER usropn
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
004000000830
004100000830     D*------------------
004200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300000830     D*------------------
004400000830     D WLBDA8          DS                  INZ
004500000830     D  G08DAT                 1      8  0
004600000830     D  G08INV                 9     16  0
004700000830     D  G08ERR                17     17
004800000830     D  G08TGI                18     22  0
004900010201
005000010201
005100990915     C                   time                    wrkdata
005200990915     C                   time                    wrkora
005300000913     C                   reset                   rrnum
005400990921     C                   reset                   esito
005500990921     C                   reset                   wrkesito
005600000724     C*
005700000724     C* SE OCCORRE SPEDIRE IN FILIALE
005800000724     C                   if        vlrpoi <> *zeros
005900000724     C*
006000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
006100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
006200000724     C     vlrpoi        chain     azorg01l
006300000724     C                   if        %found
006400000616     C                   movel(p)  CMD(1)        depcmd
006500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
006600000616     C*
006700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
006800011113     C                   Z-ADD     150           LENGH            15 5
006900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
007000000616     C                   PARM                    depcmd
007100000616     C                   PARM                    LENGH
007200000724     C*
007300000724     C                   endif
007400000724     C                   endif
007500000616     C*
007600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007700000616     C                   movel(p)  CMD(2)        depcmd
007800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
007900000616     C*
008000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
008100011113     C                   Z-ADD     150           LENGH            15 5
008200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008300000616     C                   PARM                    depcmd
008400000616     C                   PARM                    LENGH
008500000616     C*
008600071030     C                   if        not %open(titv1a7ps)
008700071030     C                   open      titv1a7ps
008800000616     C                   except    testdett
008900000613     C                   endif
009000000613     C*
009100071030     C                   exsr      opeini
009200040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
009300000613     C*
009400010202     C* Effettuo la chiamata al CLLE preposto
009500040506     C                   call(e)   'TITVVTC'
009600010202     C                   parm                    parccm
009700010202     C                   parm                    parmbr
009800010202     C                   parm      '2'           paropz
009900050201     C*
010000050201     C* Effettuo lancio TISI95 solo x chiusura
010100050201     C                   CLEAR                   TISI95DS
010200050201     C                   EVAL      I95TLA = 'C'
010300050201     C                   CALL      'TISI95R'
010400050201     C                   PARM                    TISI95DS
010500010202     C*
010600071030     C                   if        %open(titv1a7ps)
010700000616     C                   except    findett
010800071030     C                   close     titv1a7ps
010900000613     C                   endif
011000000616     C*
011100010201     C                   seton                                        LR
011200000613
011300000613
011400000613     C*--------------------------------------------------------
011500000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
011600000613     C*--------------------------------------------------------
011700000621     C     STPR          BEGSR
011800000613     C*
011900071030     C                   if        not %open(titv1a7p)
012000071030     C                   open      titv1a7p
012100990915     C                   endif
012200990915     C*
012300990915     C                   except    riepilogo
012400990915     C*
012500071030     C                   if        %open(titv1a7p)
012600071030     C                   close     titv1a7p
012700990914     C                   endif
012800990910     C*
012900000613     C                   ENDSR
013000000613     C***
013100071030
013200071030
013300071030
013400071030     C*--------------------------------------------------------
013500071030     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
013600071030     C*--------------------------------------------------------
013700071030     C     PREELA        BEGSR
013800071030     C*
013900071030     C* SE OCCORRE SPEDIRE IN FILIALE
014000071030     C                   if        invfil <> *zeros and
014100071030     C                             flgGiro = '0'
014200071030     C*
014300071030     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
014400071030     C                   eval      flgGiro = '1'
014500071030     C*
014600071030     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
014700071030     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014800071030     C     invfil        chain     azorg01l
014900071030     C                   if        %found
015000071030     C                   movel(p)  CMD(1)        depcmd
015100071030     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
015200071030     C*
015300071030     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
015400071030     C                   Z-ADD     150           LENGH            15 5
015500071030     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015600071030     C                   PARM                    depcmd
015700071030     C                   PARM                    LENGH
015800071030     C*
015900071030     C                   endif
016000071030     C*
016100071030     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016200071030     C                   movel(p)  CMD(2)        depcmd
016300071030     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
016400071030     C*
016500071030     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
016600071030     C                   Z-ADD     150           LENGH            15 5
016700071030     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016800071030     C                   PARM                    depcmd
016900071030     C                   PARM                    LENGH
017000071030     C*
017100071030     C                   if        not %open(titv1a7ps)
017200071030     C                   open      titv1a7ps
017300071030     C                   except    testdett
017400071030     C                   endif
017500071030     C*
017600071030     C                   endif
017700071030     C*
017800071030     C                   ENDSR
017900071030     C***
018000071030
018100071030
018200071030     C*--------------------------------------------------------
018300071030     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
018400071030     C*--------------------------------------------------------
018500071030     C     ENDELA        BEGSR
018600071030     C*
018700071030     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
018800071030     C*
018900071030     C                   if        %open(titv1a7ps)
019000071030     C                   except    findett
019100071030     C                   close     titv1a7ps
019200071030     C                   endif
019300071030     C*
019400071030     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019500071030     C                   Z-ADD     150           LENGH            15 5
019600071030     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019700071030     C                   PARM                    CMD(3)
019800071030     C                   PARM                    LENGH
019900071030     C*
020000071030     C                   ENDSR
020100071030     C***
020200071030
020300990908
020400000801
020500910830     C*--------------------------------------------------------
020600040526     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
020700910830     C*--------------------------------------------------------
020800040526     C     RWFILE        BEGSR
020900990910     C*
021000990914     C                   if        not %open(tivin00r)
021100990908     C                   open      tivin00r
021200990914     C                   endif
021300021113     C                   if        not %open(fivabwwr)
021400021113     C                   open      fivabwwr
021500990914     C                   endif
021600070103     C*
021700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021800020305     C                   exsr      prevat
021900010201     C*
022000010202     C                   if        chkcall = '0'
022100010202     C*
022200021113     C                   if        not %open(fivatwwr)
022300021113     C                   open      fivatwwr
022400010201     C                   endif
022500990910     C*
022600071030     C                   clear                   §CTROKVB
022700071030     C                   clear                   §CTROKVT
022800071030     C                   clear                   §CTRMO
022900071030     C                   clear                   §CTRNO
023000990910     C*
023100921023     C                   DO        *HIVAL
023200990913     C*
023300990915     C                   READ      tivin00r                               70
023400050627     C                   if        vindta > *blanks
023500000613     C                   add       1             rrnum
023600000801     C*
023700071030     C                   if        *in70 = *off and
023800000801     C                             (vinflg = *blanks
023900000801     C                              or vinflg = '0'
024000000801     C                              or vinflg = '2')
024100071030     C*
024200071030     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
024300071030     C                   if        vinflg = *blanks or vinflg = '0'
024400071030     C                   clear                   vinmsg
024500071030     C                   endif
024600071030     C*
024700071030     C                   exsr      inzvar
024800071030     C                   exsr      defcam
024900071030     C                   exsr      impvab                                       => carico  VAB
025000071030     C*
025100071030     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025200071030     C                   eval      depfil = VABLNP
025300071030     C                   exsr      repfil
025400071030     C                   if        depfil = invfil
025500071030     C                   if        vlrpoi = 999
025600071030     C                   MOVE(P)   invfil        VABFGS
025700071030     C                   else
025800071030     C                   MOVE(P)   vlrpoi        VABFGS
025900071030     C                   endif
026000071030     C*
026100071030     C                   exsr      PREELA
026200071030     C*
026300071030     C* Ebbene...
026400071030     C*
026500071030     C  N31              ADD       1             §CTROKVB          7 0
026600071030     C  N31              ADD       1             §CTROKVT          7 0
026700071030     C   32              ADD       1             §CTRMO            7 0
026800071030     C   31              ADD       1             §CTRNO            7 0
026900071030     C*
027000071030     C  N31              exsr      wrivab                                       => scarico VAB
027100071030     C  N31              exsr      exevatb                                      => write VAT-B
027200071030     C*
027300071030     C                   endif
027400071030     C*
027500071030     C                   if        *in31 = *off and
027600071030     C                             *in32 = *off
027700071030     C                   eval      vinflg = '1'
027800071030     C                   else
027900071030     C                   eval      recko = vindta
028000071030     C                   except    rigadett
028100071030     C                   eval      vinflg = '2'
028200071030     C                   endif
028300071030     C                   endif
028400071030     C*
028500071030     C                   else
028600071030     C                   eval      vinflg = '1'
028700071030     C                   endif
028800000905     C*
028900000905     C  N70              update    tivin000
029000000905     C*
029100991022     C  N70              ENDdo
029200010202     C*
029300010202     C                   endif
029400071030     C*
029500071030     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029600071030     C                   if        cntNonEl = *zeros or
029700071030     C                             flgMulti = '0'
029800071030     C* Se non ci sono record con errori ...
029900071030     C                   if        §ctrno = 0 and
030000071030     C                             §ctrmo = 0 and
030100071030     C                             flgStato <> '2'
030200071030     C* ... restituisco esito OK.
030300071030     C                   eval      wrkesito = '0'
030400071030     C                   else
030500071030     C                   if        §ctrokvb > 0
030600071030     C                   eval      wrkesito = '1'
030700071030     C                   else
030800071030     C                   if        flgOk = '0'
030900071030     C                   eval      wrkesito = '2'
031000071030     C                   else
031100071030     C                   eval      wrkesito = '6'
031200071030     C                   endif
031300071030     C                   endif
031400071030     C                   endif
031500071030     C                   else
031600071030     C                   eval      wrkesito = '9'
031700071030     C                   endif
031800071030     C*
031900071030     C                   if        %open(tivin00r)
032000071030     C                   close     tivin00r
032100071030     C                   endif
032200071030     C                   if        %open(fivabwwr)
032300071030     C                   close     fivabwwr
032400071030     C                   endif
032500071030     C                   if        %open(fivatwwr)
032600071030     C                   close     fivatwwr
032700071030     C                   endif
032800071030     C*
032900071030     C                   if        vlrpoi <> 999
033000071030     C                   eval      invfil = vlrpoi
033100071030     C                   endif
033200071030     C*
033300071030     C                   if        §ctrokvb > 0
033400071030     C                             and invfil > *zeros
033500071030     C                   exsr      invio
033600071030     C                   endif
033700071030     C*
033800071030     C                   if        flgGiro = '1'
033900071030     C                   exsr      endela
034000071030     C                   endif
034100990920     C*
034200910830     C                   ENDSR
034300000613     C***
034400010305
034500010305     C*----------------------------------------------------*
034600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
034700010305     C*----------------------------------------------------*
034800020305     C     WRIVAB        BEGSR
034900010305     C*
035000060225     C* Quindi scarico il buffer del file d testata
035100021113     C                   write     fivab000                                     => scarico il VAB
035200010305     C*
035300010305     C                   ENDSR
035400990920
035500000801     C*----------------------------------------------------*
035600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035700000801     C*----------------------------------------------------*
035800010201     C     INZVAR        BEGSR
035900000801     C*
036000070213     C                   CLEAR                   FIVAB000
036100070213     C                   CLEAR                   FIVAT000
036200070213     C*
036300040802     C                   Z-ADD     *zeros        Num5_0            5 0
036400040802     C                   MOVEL     '0'           FlgCAS            1
036500000801     C*
036600000801     C                   ENDSR
036700071030     C*----------------------------------------------------*
036800071030     C*  IMPOSTAZIONE CAMPI COSTANTI
036900071030     C*----------------------------------------------------*
037000071030     C     DEFCAM        BEGSR
037100071030     C*
037200071030     C* Imposto i valori di default...
037300071030     C                   EVAL      VABCCM = 2493851
037400071030     C                   EVAL      VABLNP = 249
037500071030     C                   EVAL      VABCTR = 000
037600071030     C                   EVAL      VABCTM = '7Q'
037700071030     C                   EVAL      VABCBO = '1'
037800071126     C                   EVAL      VABTC1 = 'A'
037900071030     C                   EVAL      VABRMO = 'PHILIPS SPA'
038000071030     C                   EVAL      VABCMO = '20052'
038100071030     C* ... e poi verifico se sono stati passati come parametri
038200071030     C                   IF        vlrppt > *blanks
038300071030     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
038400071030     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
038500071030     C                   EXSR      CHKNUM
038600071030     C                   IF        PiInt=*on
038700071030     C                   Z-ADD     PiVal         VABCCM
038800071030     C                   ENDIF
038900071030     C                   ENDIF
039000071030     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
039100071030     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
039200071030     C                   EXSR      CHKNUM
039300071030     C                   IF        PiInt=*on
039400071030     C                   Z-ADD     PiVal         VABLNP
039500071030     C                   ENDIF
039600071030     C                   ENDIF
039700071030     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
039800071030     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
039900071030     C                   EXSR      CHKNUM
040000071030     C                   IF        PiInt=*on
040100071030     C                   Z-ADD     PiVal         VABCTR
040200071030     C                   ENDIF
040300071030     C                   ENDIF
040400071030     C                   ENDIF
040500071030     C*
040600071030     C                   ENDSR
040700071030     C*----------------------------------------------------*
040800071030     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040900071030     C*----------------------------------------------------*
041000071030     C     IMPVAB        BEGSR
041100071030     C*
041200071030     C                   SETOFF                                       3132
041300071030     C*
041400071030     C* Reperimento campi ALFA
041500071030     C*
041600071030     C* Considerazioni sulla ragione sociale del destinatario da indicare
041700071030     C                   EVAL      VABRSD=%trim(%subst(vindta:29:30))
041800071030     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
041900071030     C     '@':'A'       XLATE     VABRSD        VABRSD
042000071030     C* ==
042100071030     C                   EVAL      VABIND=%trim(%subst(vindta:59:30))
042200071030     C                   EVAL      VABLOD=%trim(%subst(vindta:89:30))
042300071030     C                   EVAL      VABPRD=%trim(%subst(vindta:119:2))
042400071030     C                   EVAL      VABRMA=%trim(%subst(vindta:243:20))
042500071030     C*
042600071030     C* Reperimento campi NUMERICI
042700071030     C                   MOVEL     DATCOR        VABAAS
042800071030     C                   MOVE      DATCOR        VABMGS
042900071030     C* NSP/RMN
043000071030     C                   EVAL      PiStr=%trim(%subst(vindta:2:10))
043100071030     C                   EXSR      CHKNUM
043200071030     C                   IF        PiInt=*on
043300071030     C                   Z-ADD     PiVal         VABNSP
043400071030     C                   Z-ADD     PiVal         VABRMN
043500071030     C                   ELSE
043600071030     C                   SETON                                        31
043700071030     C                   Z-ADD     *zeros        VABNSP
043800071030     C                   Z-ADD     1             VABRMN
043900071030     C                   EVAL      vinmsg = %trimr(vinmsg)
044000071030     C                             + ' ' + 'VABNSP VABRMN'
044100071030     C                   ENDIF
044200071030     C* CAD
044300071030     C                   EVAL      PiStr=%trim(%subst(vindta:121:5))
044400071030     C                   EXSR      CHKNUM
044500071030     C                   IF        PiInt=*on
044600071030     C                   Z-ADD     PiVal         Num5_0
044700071030     C                   MOVEL(p)  Num5_0        VABCAD
044800071030     C                   ELSE
044900071030     C                   SETON                                        32
045000071030     C                   EVAL      VABCAD = *zeros
045100071030     C                   EVAL      vinmsg = %trimr(vinmsg)
045200071030     C                             + ' ' + 'VABCAD'
045300071030     C                   ENDIF
045400071030     C* Reperisco la provincia dal CAP e dalla località
045500071030     C                   IF        VABCAD <> *blanks AND
045600071030     C                             VABPRD  = *blanks
045700071030     C                   CLEAR                   TISI95DS
045800071030     C                   EVAL      I95TCN = '3'
045900071030     C                   Z-ADD     datcor        I95DAT
046000071030     C                   EVAL      I95CAP = VABCAD
046100071030     C                   EVAL      I95LOC = VABLOD
046200071030     C                   EVAL      I95NAR = VABNZD
046300071030     C                   CALL      'TISI95R'
046400071030     C                   PARM                    TISI95DS
046500071030     C                   EVAL      VABPRD = O95PRV
046600071030     C                   ENDIF
046700071030     C* NCL
046800071030     C                   EVAL      PiStr=%trim(%subst(vindta:136:5))
046900071030     C                   EXSR      CHKNUM
047000071030     C                   IF        PiInt=*on
047100071030     C                   Z-ADD     PiVal         VABNCL
047200071030     C                   ELSE
047300071030     C                   SETON                                        32
047400071030     C                   Z-ADD     *zeros        VABNCL
047500071030     C                   EVAL      vinmsg = %trimr(vinmsg)
047600071030     C                             + ' ' + 'VABNCL'
047700071030     C                   ENDIF
047800071030     C* PKB
047900071030     C                   EVAL      PiStr=%trim(%subst(vindta:129:7))
048000071030     C                   EVAL      PiDecChr = '.'
048100071030     C                   EXSR      CHKNUM
048200071030     C                   IF        PiNum=*on
048300071030     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
048400071030     C                   Z-ADD(H)  PiVal         VABPKB
048500071030     C                   ELSE
048600071030     C                   SETON                                        32
048700071030     C                   Z-ADD     *zeros        VABPKB
048800071030     C                   EVAL      vinmsg = %trimr(vinmsg)
048900071030     C                             + ' ' + 'VABPKB'
049000071030     C                   ENDIF
049100071030     C* CAS
049200071030     C                   IF        %subst(vindta:161:9) <> *zeros
049300071030     C                   EVAL      FlgCAS = '1'
049400071030     C                   EVAL      PiStr=%trim(%subst(vindta:161:9))
049500071030     C                   EXSR      CHKNUM
049600071030     C                   IF        PiNum=*on
049700071030     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
049800071030     C                   Z-ADD(H)  PiVal         VABCAS
049900071030     C                   EVAL      VABVCA = 'EUR'
050000071030     C                   EVAL      VABTIC = 'OM'
050100071030     C                   ELSE
050200071030     C                   SETON                                        32
050300071030     C                   Z-ADD     *zeros        VABCAS
050400071030     C                   EVAL      vinmsg = %trimr(vinmsg)
050500071030     C                             + ' ' + 'VABCAS'
050600071030     C                   ENDIF
050700071030     C                   ENDIF
050800071030     C*
050900071030     C* Considerazioni finali su CBO/CAS
051000071030     C                   IF        FlgCAS = '1'
051100071030     C                   IF        VABCBO = '1'
051200071030     C                   EVAL      VABCBO = '4'
051300071030     C                   ENDIF
051400071030     C                   IF        VABCBO = '2'
051500071030     C                   EVAL      VABCBO = '6'
051600071030     C                   ENDIF
051700071030     C                   ENDIF
051800071030     C*
051900071030     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052000071030     C                   EXSR      CHKIMPDIV
052100071030     C*
052200071030     C                   ENDSR
052300070102     C*----------------------------------------------------*
052400071030     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
052500070102     C*----------------------------------------------------*
052600071030     C     EXEVATB       BEGSR
052700070102     C*
052800071030     C                   EVAL      VATFGS = VABFGS
052900071030     C                   EVAL      VATAAS = VABAAS
053000071030     C                   EVAL      VATLNP = VABLNP
053100071030     C                   EVAL      VATNRS = VABNRS
053200071030     C                   EVAL      VATNSP = VABNSP
053300071030     C                   EVAL      VATCCM = VABCCM
053400071030     C                   EVAL      VATTRC='B'
053500071030     C                   EVAL      VATNOT = %trim(%subst(vindta:264:30))
053600071019     C* Occorre togliere gli spazi eventualmente presenti all'interno della stringa barcode
053700071019     C                   Z-ADD     2             i                 2 0
053800071019     C                   DOW       i < %size(VATNOT)
053900071019     C                   IF        %subst(VATNOT:i:1) = *blanks
054000071019     C                   EVAL      VATNOT = %subst(VATNOT:1:i-1) +
054100071019     C                                      %subst(VATNOT:i+1)
054200071019     C                   ENDIF
054300071019     C                   ADD       1             i
054400071019     C                   ENDDO
054500071019     C*
054600070102     C                   exsr      wrivat                                       => scarico VAT
054700070102     C*
054800070102     C                   ENDSR
054900010201     C*----------------------------------------------------*
055000040802     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
055100010201     C*----------------------------------------------------*
055200020305     C     WRIVAT        BEGSR
055300050628     C*
055400060223     C* Scrivo solo se valorizzato qualcosa
055500060223     C                   IF        VATNOT <> *blanks
055600040802     C                   WRITE     FIVAT000
055700060223     C                   ENDIF
055800010201     C*
055900010201     C                   ENDSR
056000010202     C*----------------------------------------------------*
056100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
056200010202     C*----------------------------------------------------*
056300020305     C     PREVAT        BEGSR
056400010202     C*
056500021113     C* Compongo il nome del membro da dare al FIVATWWR
056600010202     C                   eval      parmbr = vlrhdl
056700010202     C                   movel     'M'           parmbr
056800050627     C                   eval      parccm = %subst(vlrKSC:2:7)
056900010202     C                   eval      paropz = '1'
057000010202     C* Effettuo la chiamata al CLLE preposto
057100040506     C                   call(e)   'TITVVTC'
057200010202     C                   parm                    parccm
057300010202     C                   parm                    parmbr
057400010202     C                   parm                    paropz
057500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
057600010202     C                   if        %error
057700010202     C                   movel     '1'           chkcall
057800010202     C                   else
057900010202     C                   movel     '0'           chkcall
058000010202     C                   endif
058100010202     C*
058200010202     C                   ENDSR
058300000801     C*----------------------------------------------------*
058400000801     C*  CONTROLLO NUMERICITA' CAMPI
058500000801     C*----------------------------------------------------*
058600000801     C     CHKNUM        BEGSR
058700071030     C*
058800071030     C                   IF        PiDecChr = *blanks
058900071030     C                   EVAL      PiDecChr = '.'
059000071030     C                   ENDIF
059100000801     C*
059200000801     C                   call(e)   'ISNUMERIC'
059300000801     C                   PARM                    PiStr            30
059400071030     C                   PARM                    PiDecChr          1
059500000801     C                   PARM      *ZEROS        PiVal            30 9
059600000801     C                   PARM      '0'           PiInt             1
059700000801     C                   PARM      '0'           PiNum             1
059800000801     C                   IF        %error
059900000801     C                   EVAL      PiInt=*off
060000000801     C                   ENDIF
060100000801     C*
060200000801     C                   ENDSR
060300000801     C***
060400000801
060500011113
060600011113     C*----------------------------------------------------*
060700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
060800011113     C*----------------------------------------------------*
060900011113     C     CHKIMPDIV     BEGSR
061000011113     C*
061100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
061200011113     C                   Z-ADD     *zeros        wrkDec            9 9
061300011113     C*
061400011113     C* Come prima cosa effettuo considerazioni sulla divisa
061500011113     C                   IF        vabIAS > *zeros
061600011113     C                   IF        vabVAS <> 'EUR'
061700011113     C                   EVAL      vabVAS =  'ITL'
061800011113     C                   ENDIF
061900011113     C                   ENDIF
062000011113     C*
062100011113     C                   IF        vabCAS > *zeros
062200011113     C                   IF        vabVCA <> 'EUR'
062300011113     C                   EVAL      vabVCA =  'ITL'
062400011113     C                   ENDIF
062500011113     C                   ENDIF
062600011113     C*
062700011113     C                   IF        vabVMD > *zeros
062800020305     C                   IF        vabVAD <> 'EUR'
062900011113     C                   EVAL      vabVAD =  'ITL'
063000011113     C                   ENDIF
063100011113     C                   ENDIF
063200011113     C*
063300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
063400011113     C                   Z-ADD     vabIAS        wrkDec
063500011113     C                   IF        wrkDec > *zeros
063600011113     C                   IF        vabVAS = 'ITL'
063700011113     C                   EVAL      vabIAS = *zeros
063800011113     C                   ENDIF
063900011113     C                   ENDIF
064000011113     C*
064100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
064200011113     C                   Z-ADD     vabCAS        wrkDec
064300011113     C                   IF        wrkDec > *zeros
064400011113     C                   IF        vabVCA = 'ITL'
064500011113     C                   EVAL      vabCAS = *zeros
064600011113     C                   ENDIF
064700011113     C                   ENDIF
064800011113     C*
064900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065000011113     C                   Z-ADD     vabVMD        wrkDec
065100011113     C                   IF        wrkDec > *zeros
065200011113     C                   IF        vabVAD = 'ITL'
065300011113     C                   EVAL      vabVMD = *zeros
065400011113     C                   ENDIF
065500011113     C                   ENDIF
065600011113     C*
065700011113     C                   ENDSR
065800011113     C***
065900011113
066000071030
066100071030
066200071030      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
066300071030     C     repfil        BEGSR
066400071030     C*
066500071030     C                   if        invfil = *zeros and
066600071030     C                             depfil > *zeros and
066700071030     C                             (vinflg = *blanks or
066800071030     C                              vinflg = *zeros)
066900071030     C
067000071030     C                   eval      invfil = depfil
067100071030     C                   endif
067200071030     C*
067300071030     C                   if        depfil <> invfil and
067400071030     C                             invfil > *zeros
067500071030     C                   eval      flgMulti = '1'
067600071030     C                   if        vinflg = *blanks
067700071030     C                   add       1             cntNonEl
067800071030     C                   endif
067900071030     C                   endif
068000071030     C*
068100071030     C                   if        vinflg = '2'
068200071030     C                   eval      flgStato = '2'
068300071030     C                   endif
068400071030     C*
068500071030     C                   ENDSR
068600071030     C***
068700011113
068800000801
068900000801
069000990920      /TITLE Invio dei dati al punto operativo.
069100010202     C     invio         BEGSR
069200990920     C*
069300021113     C* 1° invio FIVAT
069400010201     C                   reset                   dscmz
069500010201     C                   move      vlrpoi        cmzdst
069600021113     C                   eval      cmzfld = 'FIVATWWR'
069700010201     C                   eval      cmzmbd = vlrhdl
069800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069900021009     C***                if        prmfir = *blanks
070000021113     C                   eval      cmzfla = 'FIVAT00F'
070100021113     C                   eval      cmzmba = 'FIVAT00F'
070200021009     C***                else
070300021009     C***                eval      cmzfla = prmfir
070400021009     C***                eval      cmzmba = prmfir
070500021009     C***                endif
070600010201     C                   eval      cmznrr = *zeros
070700020305     C                   move      §ctrokvt      cmznrr
070800021018     C                   eval      cmzlba = vlrfl1
070900010201     C                   call(e)   'TIS711C'
071000010201     C                   parm                    dscmz
071100010201     C                   parm      *blanks       esito
071200010205     C                   if        %error
071300010205     C                             or cmzerr = '1'
071400010205     C                             or esito  = '1'
071500010205     C                   eval      wrkesito = '3'
071600010205     C                   else
071700010201     C*
071800021113     C* 2° invio FIVAB
071900010201     C                   reset                   dscmz
072000010201     C                   move      vlrpoi        cmzdst
072100010201     C                   eval      cmzfld = vlrfou
072200010201     C                   eval      cmzmbd = vlrhdl
072300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072400021009     C***                if        prmfir = *blanks
072500021113     C                   eval      cmzfla = 'FIVAB00F'
072600021113     C                   eval      cmzmba = 'FIVAB00F'
072700021009     C***                else
072800021009     C***                eval      cmzfla = prmfir
072900021009     C***                eval      cmzmba = prmfir
073000021009     C***                endif
073100010201     C                   eval      cmznrr = *zeros
073200010201     C                   move      §ctrokvb      cmznrr
073300021018     C                   eval      cmzlba = vlrfl1
073400010201     C                   call(e)   'TIS711C'
073500010201     C                   parm                    dscmz
073600010201     C                   parm      *blanks       esito
073700010201     C                   if        %error
073800010201     C                             or cmzerr = '1'
073900010201     C                             or esito  = '1'
074000010201     C                   eval      wrkesito = '3'
074100010201     C                   endif
074200010205     C                   endif
074300990920     C*
074400000613     C                   ENDSR
074500000613     C***
074600070411
074700070411     C     *pssr         BEGSR
074800070411     C*
074900070411     C                   if        %open(tivin00r)
075000070411     C                   close     tivin00r
075100070411     C                   endif
075200070411     C                   if        %open(fivabwwr)
075300070411     C                   close     fivabwwr
075400070411     C                   endif
075500070411     C                   if        %open(fivatwwr)
075600070411     C                   close     fivatwwr
075700070411     C                   endif
075800070411     C*
075900070411     C* Effettuo la chiamata al CLLE preposto
076000070411     C                   call(e)   'TITVVTC'
076100070411     C                   parm                    parccm
076200070411     C                   parm                    parmbr
076300070411     C                   parm      '2'           paropz
076400070411     C*
076500070411     C                   eval      wrkesito = '2'
076600070411     C*
076700070411     C                   seton                                        LR
076800070411     C*
076900070411     C                   ENDSR     '*CANCL'
077000070411     C***
077100070411
077200071030
077300071030
077400071030      /TITLE Invio dei dati al punto operativo.
077500071030     C     opeini        BEGSR
077600071030     C*
077700071030     C* Inizializzo flag e contatori operativi
077800071030     C                   movel     '0'           flgGiro           1
077900071030     C                   movel     '0'           flgMulti          1
078000071030     C                   movel     '1'           flgStato          1
078100071030     C                   movel     '0'           flgOk             1
078200071030     C                   z-add     *zeros        cntNonEl         10 0
078300071030     C                   z-add     *zeros        depfil            3 0
078400071030     C                   z-add     *zeros        invfil            3 0
078500071030     C*
078600071030     C                   ENDSR
078700071030     C***
078800071030
078900071030
079000990910
079100000613     C     *inzsr        BEGSR
079200990910     C*
079300990910     C     *entry        plist
079400990920     C                   parm                    tivlrds
079500990921     C                   parm      wrkesito      esito
079600000724     C                   parm                    prmlit
079700000710     C                   parm                    prmfir
079800000613     C*
079900000830     C* CALCOLA LA DATA CORRENTE
080000000830     C                   time                    wn14             14 0
080100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
080200000830     C                   z-add     wn8           g08dat
080300000830     C                   z-add     *zeros        g08inv
080400000830     C                   movel     '0'           g08err
080500000830     C                   call      'XSRDA8'
080600000830     C                   parm                    wlbda8
080700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
080800000830     C*
080900000613     C                   ENDSR
081000000613     C***
081100990908
081200071030     Otitv1a7p  E            riepilogo         2
081300990915     O                                              'Upload via Internet'
081400990915     O                                           +1 'Traduzione TIVIN00R -'
081500021113     O                                           +1 'FIVABWWR/FIVATWWR'
081600010201     O                                           +1 'Report testate bolle'
081700990915     O          E            riepilogo   2
081800990915     O                       wrkdata
081900990915     O                       wrkora              +1
082000990915     O                       procname            +1
082100990915     O          E            riepilogo   2
082200990915     O                                              'Cliente..................:'
082300990915     O                       VABCCM        z     +1
082400990915     O          E            riepilogo   2
082500990920     O                                              'Riferimento Strategi.....:'
082600990920     O                       vlrhdl              +1
082700990915     O          E            riepilogo   2
082800990915     O                                              'Giusti...................:'
082900010201     O                       §CTROKVB      2   +  1
083000990915     O          E            riepilogo   2
083100990915     O                                              'Sbagliati e corretti.....:'
083200971022     O                       §CTRMO        2   +  1
083300990915     O          E            riepilogo   2
083400990915     O                                              'Sbagliati e scartati.....:'
083500971022     O                       §CTRNO        2   +  1
083600000613
083700071030     Otitv1a7ps E            testdett          2
083800000613     O                                              'Upload via Internet'
083900000613     O                                           +1 'Traduzione TIVIN00R -'
084000021113     O                                           +1 'FIVABWWR/FIVATWWR'
084100010201     O                                           +1 'Report testate bolle'
084200000616     O          E            testdett    3
084300000613     O                                           +2 'N° rec'
084400000613     O                                           +3 'Anteprima contenuto'
084500000616     O          E            rigadett    2
084600000613     O                       rrnum               +2
084700000621     O                       recko               +3
084800000616     O          E            findett     2
084900000613     O                       wrkdata
085000000613     O                       wrkora              +1
085100000613     O                       procname            +1
085200000616     O          E            findett     2
085300000613     O                                              'Cliente..................:'
085400000613     O                       VABCCM        z     +1
085500000616     O          E            findett     2
085600000613     O                                              'Riferimento Strategi.....:'
085700000613     O                       vlrhdl              +1
085800000616     O          E            findett     2
085900000613     O                                              'Giusti...................:'
086000010201     O                       §CTROKVB      2   +  1
086100000616     O          E            findett     2
086200000613     O                                              'Sbagliati e corretti.....:'
086300000613     O                       §CTRMO        2   +  1
086400000616     O          E            findett     2
086500000613     O                                              'Sbagliati e scartati.....:'
086600000613     O                       §CTRNO        2   +  1
086700000512** CMD - COMANDI CL
086800071030OVRPRTF FILE(TITV1A7P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
086900071030OVRPRTF FILE(TITV1A7PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
087000071030DLTOVR FILE(TITV1A7P TITV1A7PS) LVL(*)
087100000512
087200000512
