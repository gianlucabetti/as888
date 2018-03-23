000100140228      * --------------------------------------------------------------*
000200080228      *?INTERROGAZIONE ORGANIGRAMMA                                  ?*
000300080228      *---------------------------------------------------------------*
000400080228     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500080228      *
000600080228      *?  A R C H I V I  ?--------------------------------------------*
000700080228      *
000800140313     fAZORG01L  uf   e           k disk    usropn
000900080228     fTABEL00F  if   e           k disk
001000080228      *
001100140228     ffidg98nd  cf   e             workstn
001200080228      *
001300080228      *?  C O S T A N T I  ?------------------------------------------*
001400080228      *
001500080228      *?  S C H I E R E  ?--------------------------------------------*
001600080228      * - Messaggi di errore
001700140905     d $Msg            s             78    dim(06)  ctdata  perrcd(1)
001800140620     d cmd             s             80    dim(4)  ctdata perrcd(1)
001900080228      *
002000080228      *?  S T R U T T U R E   D A T I  ?------------------------------*
002100080228      *
002200080228      * Parametri ricevuti
002300080228     d KPJBA         e ds
002400140305     d Og146         e ds                  inz
002500140305     d Og147         e ds                  inz
002600140228     d Og148         e ds                  inz
002700080228      *
002800080228      * - Parametri x Controllo profilo utenti
002900080228     d TIBS34ds      e ds                  inz
003000080228      * - Ds di riferimento al file esterno AZUTE00F
003100080228     d AZUTEds       e ds                  extname(AZUTE00F)
003200080228      * - Ds per dati organigramma
003300080228     d dDatiUte      e ds
003400080228      * - Abilitazioni utenti aziendali
003500080228     d dUte01        e ds
003600140619     d tibs55ds      e ds                  inz
003700140613     d fnlv55ds      e ds                  inz
003800130830
003900080228      * - Controllo/Inversione date
004000080228     d WLBdat          ds                  inz
004100080228     d  G02dat                 1      8  0 inz
004200080228     d  G02inv                 9     16  0 inz
004300080228     d  G02err                17     17    inz('3')
004400080228     d  G02tgi                18     22  0 inz
004500080228      *
004600080228      *?  V A R I A B I L I  ?----------------------------------------*
004700140228      * - Parametri per TNSD24R
004800140228     d §§Cod           s              3    inz
004900140306     d §§tip           s                   like(ORGfag)  inz
005000140228     d §§Des           s             25    inz
005100140228     d §§Pra           s              3    inz
005200080228      *
005300080228      * - Flag booleani
005400080228      *
005500140807     d wforza          s               n
005600080228      * - Indici di schiera
005700080228     d xa              s              3  0 inz
005800080228      *  - Comodo
005900080228     d §Fil            s                   like(ORGfil)  inz
006000080228     d wFil            s                   like(ORGfil)  inz
006100130830     d Datasys         s               d   datfmt(*iso) inz(*sys)
006200130830     d klna            s              3  0 inz
006300140313     d command         s             80
006400140313     d lung            s             15  5 inz(80)
006500140620      *
006600140620     D Corg            C                   CONST('/AZORG01L)')
006700140620     D Carb            C                   CONST('/fnarb00f) SECURE(*YES)')
006800140620     D Carg            C                   CONST('/fnarg00f) SECURE(*YES)')
006900140620     D Cdst            C                   CONST('/fidst01l) SECURE(*YES)')
007000080228      *
007100080228      *?  M A I N - L I N E  ?----------------------------------------*
007200080228      *
007300080228     c     *Entry        plist
007400080228     c                   parm                    KPJBA
007500140619      * reperimento libreria sistema informativo
007600140619     C                   CLEAR                   tibs55ds
007700140619     c                   movel     'L'           i50tla
007800140619     c                   Movel     046           I50apo
007900140619     C                   CALL      'TIBS55R'                              01
008000140619     C                   PARM                    tibs55ds
008100140619     C                   movel     o50alg        libsede          10
008200140619     c
008300140619     C                   CLEAR                   tibs55ds
008400140619     c                   movel     'L'           i50tla
008500140619     c                   Movel     001           I50apo
008600140619     C                   CALL      'TIBS55R'
008700140619     C                   PARM                    tibs55ds
008800140619     C                   movel     o50alg        libfiliale       10
008900140619     C                   movel     o50ala        libfilfil        10
009000140619     C
009100140313      * ovrdbf azorg01l di filiale
009200140313     c                   clear                   command
009300140619     c                   movea     cmd(1)        command
009400140619     c                   eval      %subst(command:30 :10) = libfiliale
009500140620     c                   eval      command=%trim(command) + Corg
009600140313     c                   call      'QCMDEXC'
009700140313     c                   parm                    command
009800140313     c                   parm                    lung
009900140313     c                   open      azorg01l
010000131021      *
010100140313      *
010200140305     C                   TIME                    W0140            14 0
010300140305     C                   MOVE      W0140         UDATE8            8 0
010400140305     C                   MOVEL     W0140         HHMM              4 0          *ORA/MINUTI
010500140305     C*
010600140305     C                   Z-ADD     UDATE8        G02DAT                         *GG/MM/AAAA
010700140305     C                   MOVEL     *BLANK        G02ERR
010800140305     C                   CALL      'XSRDA8'
010900140305     C                   PARM                    WLBDAT
011000140305     C                   Z-ADD     G02INV        DATEU8            8 0          *AAAA/MM/GG
011100080228      *
011200080228      * Reperisco dati job
011300080228     c                   exsr      DatiJob
011400080228      *
011500080228      * 88 on - organigramma di sede
011600080228     c                   eval      *in88 = (%subst(KPJBU : 1 : 4) = 'SEDE')
011700080228     c                   eval      TBLkut = 1
011800080228      *
011900080228      *---------------------------------------------------------------------
012000030430      *
012100000000     C     INIZIO        TAG
012200080228     C                   movel     *blanks       VFil
012300030430      *
012400000000     C     VID1          TAG
012500140306     c                   setoff                                       808182
012600140613     c                   setoff                                       838440
012700140228     C                   EXFMT     video1
012800000000     C   KC              GOTO      FINE
012900130904      *
013000030430      *
013100030430      * Gestione VIDEO 1
013200030430      *
013300000000     C     VFIL          IFEQ      *ZERO
013400000000     C                   SETON                                        50
013500000000     C                   GOTO      VID1
013600000000     C                   END
013700000000     C*
013800080228     C     '?'           scan      VFil                                   21
013900000000     C   21              DO
014000080228     C   88              MOVE      '§'           §§Tip
014100971203     C                   CALL      'TNSD24R'
014200080228     C                   PARM                    §§Cod
014300080228     C                   PARM                    §§Tip
014400080228     C                   PARM                    §§DES
014500080228     C                   PARM                    §§PRA
014600080228     C                   MOVEL     §§COD         VFIL
014700000000     C                   GOTO      VID1
014800080228     C                   ENDdo
014900000000     C** CONTROLLO CHE IL CODICE FILIALE IMMESSO SIA
015000000000     C** NUMERICO
015100040922     C                   TESTN                   VFIL                 36
015200040922     c                   move      vfil          w001a             1
015300040922     c
015400040922     c                   if        not *in36 or w001a<'0'
015500000000     C                   SETON                                        50
015600000000     C                   GOTO      VID1
015700000000     C                   END
015800000000     C*
015900080228     C                   MOVEL     VFIL          §Fil
016000140807     C     §Fil          CHAIN     AZORG
016100140807     c                   if        not %found(azorg01l) or orgfva <> *blanks or
016200140807     c                             (orgfag <> 'A' and orgfag <> 'F')
016300000000     C                   SETON                                        50
016400000000     C                   GOTO      VID1
016500040922     C                   endif
016600140305     C                   MOVEL     ORGDE6        OG146
016700140305     C                   MOVEL     ORGDE7        OG147
016800140228     C                   MOVEL     ORGDE8        OG148
016900140305     C                   EXSR      pulVid2
017000140807     C                   if        §ogtlls = ' '
017100140807     c                   seton                                        818283
017200140807     c                   seton                                        84
017300140807     c                   else
017400140305     C                   EXSR      AggVid2
017500140807     c                   endif
017600040303     c
017700080228      * Descrizione p.o.
017800140305     c                   eval      VDFil = orgdes
017900140305     c     sopra         tag
018000140305     c                   setoff                                       90
018100130423
018200140305     c                   do        *hival
018300140228     C                   EXFMT     video2
018400140305     c                   setoff                                       28
018500080228      * Fine lavoro
018600080228     C   KC              GOTO      INIZIO
018700070828      * Video precedente
018800140228     C   KL              GOTO      VID1
018900140305     c                   exsr      contr2
019000030430      *
019100140305     C   90              GOTO      sopra
019200140313     c                   if        not *in80 or not *in81 or not *in82 or
019300140313     c                             not *in83 or  not *in84
019400140305     c   kf              exsr      aggiorna
019500140306     C   kf              GOTO      VID1
019600140313     c                   endif
019700140305     c                   enddo
019800000000     C     FINE          TAG
019900140616      *cancella eventuali ovrdbf per chiamate secondarie
020000140616     c                   clear                   command
020100140616     c                   eval      command = 'DLTOVR FILE(*ALL) LVL(*JOB)'
020200140616     c                   call      'QCMDEXC'
020300140616     c                   parm                    command
020400140616     c                   parm                    lung
020500030430      *
020600000000     C                   SETON                                        LR
020700080228      *
020800080228      *---------------------------------------------------------------*
020900080228      * Reperimento Dati del job (Utente/Operativi)                   *
021000080228      *---------------------------------------------------------------*
021100080228     c     DatiJob       BEGSR
021200080228      *
021300080228     c     *dtaara       define    §azute        azuteds
021400080228     c     *dtaara       define    §datiute      ddatiute
021500080228      *
021600080228     c                   in(E)     *dtaara
021700080228     c                   IF        %ERROR or RSUT = *blanks
021800080228     c                   clear                   Tibs34Ds
021900080228     c                   call      'TIBS34R'
022000080228     c                   parm                    Tibs34Ds
022100080228     c                   in        *dtaara
022200080228     c                   ENDIF
022300080228      *
022400080228     c                   eval      dUTE01 = UTEfaf
022500080228      *
022600080228     c                   ENDSR
022700140613      *------------------------------------------------------
022800140613      * PartCons
022900140613      *------------------------------------------------------
023000140613     C     partCons      BEGSR
023100140613     c*ovrdbf per fnarb per chiamato
023200140613     c                   clear                   command
023300140620     c                   movea     cmd(2)        command
023400140619     c                   eval      %subst(command:30 :10) = libfilfil
023500140620     c                   eval      command=%trim(command) + Carb
023600140613     c                   call      'QCMDEXC'
023700140613     c                   parm                    command
023800140613     c                   parm                    lung
023900140613     c*ovrdbf per fnarg per chiamato
024000140613     c                   clear                   command
024100140620     c                   movea     cmd(3)        command
024200140619     c                   eval      %subst(command:30 :10) = libfilfil
024300140620     c                   eval      command=%trim(command) + Carg
024400140613     c                   call      'QCMDEXC'
024500140613     c                   parm                    command
024600140613     c                   parm                    lung
024700140613     c*
024800140616     c                   call      'FIDG99R'
024900140613     c                   parm                    vfil
025000140613     c                   ENDSR
025100140613      *------------------------------------------------------
025200140613      * PartRit
025300140613      *------------------------------------------------------
025400140613     C     partRit       BEGSR
025500140613     c                   call      'FIDG94R'
025600140613     c                   parm                    vfil
025700140613     c*ovrdbf per fnarb per chiamato
025800140613     c                   clear                   command
025900140620     c                   movea     cmd(4)        command
026000140619     c                   eval      %subst(command:30 :10) = libfilfil
026100140620     c                   eval      command=%trim(command) + Cdst
026200140613     c                   call      'QCMDEXC'
026300140613     c                   parm                    command
026400140613     c                   parm                    lung
026500140613     c                   call      'TNSY01R'
026600140613     c                   parm                    vfil
026700140613     c                   ENDSR
026800070828      *------------------------------------------------------
026900140305      * Pulizia VIDEO 2
027000070828      *------------------------------------------------------
027100140305     C     PULVID2       BEGSR
027200070828      *
027300140305     C                   movel     *blanks       Vgeot
027400070828     C                   z-add     0             Vdda
027500070828     c                   z-add     0             vddao
027600070828     C                   movel     *blanks       Vtlls
027700070828     c                   movel     *blanks       vcgio
027800070828      *
027900070828     C                   ENDSR
028000140305      *------------------------------------------------------
028100140305      * aggiorna
028200140305      *------------------------------------------------------
028300140305     C     aggiorna      BEGSR
028400140909     c                   clear                   wforza
028500140313      * chiudo il file aperto
028600140313     c                   if        %open(azorg01l)
028700140313     c                   close     azorg01l
028800140313     c                   endif
028900140313
029000140313      * ovrdbf azorg01l di filiale
029100140313     c                   clear                   command
029200140619     c                   movea     cmd(1)        command
029300140619     c                   eval      %subst(command:30 :10) = libfiliale
029400140620     c                   eval      command=%trim(command) + Corg
029500140313     c                   call      'QCMDEXC'
029600140313     c                   parm                    command
029700140313     c                   parm                    lung
029800140313     c                   open      azorg01l
029900140313
030000140313      * aggiorno organigramma di filiale
030100140313     c     §fil          chain     azorg01l
030200140313     c                   if        %found(azorg01l)
030300140613     C                   MOVEL     ORGDE6        OG146
030400140613     C                   MOVEL     ORGDE7        OG147
030500140613     C                   MOVEL     ORGDE8        OG148
030600140313     C                   move      Vgeot         §oggeot
030700140613      *partenza consegne
030800140613     c                   if        §ogdda = *blank and wvdda <> 0
030900140613     c                   exsr      partCons
031000140613     c                   endif
031100140313     C                   move      wvdda         §ogdda
031200140613     c                   if        §ogddao = *blank and wvddao <> 0
031300140613     c                   exsr      partRit
031400140613     c                   endif
031500140313     c                   move      wvddao        §ogddao
031600140313     C                   movel     Vtlls         §ogtlls
031700140313     c                   movel     vcgio         §ogcgio
031800140313     C                   MOVEL(p)  og146         ORGDE6
031900140313     C                   MOVEL(p)  og147         ORGDE7
032000140313     C                   MOVEL(p)  og148         ORGDE8
032100140313     c                   update    azorg
032200140313     c                   endif
032300140313
032400140313      * chiudo il file aperto
032500140313     c                   if        %open(azorg01l)
032600140313     c                   close     azorg01l
032700140313     c                   endif
032800140313
032900140313      * ovrdbf azorg01l di sede
033000140313     c                   clear                   command
033100140620     c                   movea     cmd(1)        command
033200140619     c                   eval      %subst(command:30 :10) = libsede
033300140620     c                   eval      command=%trim(command) + Corg
033400140313     c                   call      'QCMDEXC'
033500140313     c                   parm                    command
033600140313     c                   parm                    lung
033700140313     c                   open      azorg01l
033800140313
033900140313      * imposto subito il flag in organigramma di filiale
034000140313     c     §fil          chain     azorg01l
034100140313     c                   if        %found(azorg01l)
034200140613     C                   MOVEL     ORGDE6        OG146
034300140613     C                   MOVEL     ORGDE7        OG147
034400140613     C                   MOVEL     ORGDE8        OG148
034500140313     C                   move      Vgeot         §oggeot
034600140313     C                   move      wvdda         §ogdda
034700140313     c                   move      wvddao        §ogddao
034800140313     C                   movel     Vtlls         §ogtlls
034900140313     c                   movel     vcgio         §ogcgio
035000140313     C                   MOVEL(p)  og146         ORGDE6
035100140313     C                   MOVEL(p)  og147         ORGDE7
035200140313     C                   MOVEL(p)  og148         ORGDE8
035300140313     c                   update    azorg
035400140313     c                   endif
035500140305      *
035600140305     C                   ENDSR
035700070828      *-----------------------------------------------------
035800140305      * Riempimento  video 2
035900070828      *-----------------------------------------------------
036000140305     C     AGGVID2       BEGSR
036100070828      *
036200140807      * TELLUS
036300140807     C                   MOVE      §ogtlls       Vtlls
036400140807     c                   if        vtlls = 'S'
036500140807     c                   seton                                        80
036600140807     c                   endif
036700140807     c                   if        vtlls = 'D'
036800140807     c                   setoff                                       818283
036900140807     c                   setoff                                       84
037000140807     c                   endif
037100070828      * flag avviamento gestione telefonate per GEO
037200070828     c                   movel     §oggeot       vgeot
037300140306     c                   if        vgeot = 'S'
037400140313     c                   seton                                        81
037500140306     c                   endif
037600070828     c* data inizio distinta di consegne automatica
037700140807     c                   if        §ogdda<>*blanks and
037800140807     c                             §ogdda<>'00000000'
037900140313     c                   seton                                        82
038000070828     C                   CLEAR                   WLBDAT
038100070828     C                   MOVEL     '3'           G02ERR
038200070828     C                   move      §ogdda        G02INV
038300070828     C                   CALL      'XSRDA8'
038400070828     C                   PARM                    WLBDAT
038500070828     C                   Z-ADD     G02DAT        Vdda
038600070828     c                   endif
038700070828      * Procedura GIRI su ORM
038800070828     c                   move      §ogcgio       vcgio
038900140306     c                   if        vcgio = 'S'
039000140313     c                   seton                                        83
039100140306     c                   endif
039200070828      * Data inizio distinta ORM automatica
039300140807     c                   if        §ogddao <> *blanks and
039400140807     c                             §ogddao<>'00000000'
039500140313     c                   seton                                        84
039600070828     c                   clear                   wlbdat
039700070828     c                   eval      g02err = '3'
039800070828     c                   move      §ogddao       g02inv
039900070828     c                   call      'XSRDA8'
040000070828     c                   parm                    wlbdat
040100070828     c                   z-add     g02dat        vddao
040200070828     c                   endif
040300070828     C                   ENDSR
040400140305      *-----------------------------------------------------
040500140305      * controlli    video 2
040600140305      *-----------------------------------------------------
040700140305     C     Contr2        begsr
040800140305      *
040900140305     C                   if        §ogtlls = *blank and
041000140305     c                             (vtlls <> *blank and vtlls <> 'D')
041100140305     c                   seton                                        2890
041200140305     c                   movel     $msg(2)       v1dmsg
041300140305     c                   leavesr
041400140305     c                   endif
041500140305      * flag avviamento gestione telefonate per GEO
041600140305     c                   if        vgeot = 'S' and vtlls = ' '
041700140305     c                   seton                                        2890
041800140305     c                   movel     $msg(3)       v1dmsg
041900140305     c                   leavesr
042000140305     c                   endif
042100140905     c* è secondo step verifico che tutti i campi siano pieni devono essere inseriti tutti
042200140905     c                   if        vtlls = 'S'
042300140905     c                   if        vdda  = 0 or
042400140905     c                             vddao = 0 or
042500140905     c                             vcgio = *blank or
042600140905     c                             vcgio = *blank or
042700140905     c                             vgeot = *blank
042800140905     c                   seton                                        2890
042900140905     c                   movel     $msg(6)       v1dmsg
043000140905     c                   leavesr
043100140905     c                   endif
043200140905     c                   endif
043300140305     c* data inizio distinta di consegne automatica
043400140305     c                   if        vdda > 0
043500140305     C                   CLEAR                   WLBDAT
043600140305     C                   MOVEL     '1'           G02ERR
043700140305     C                   move      vdda          G02dat
043800140305     C                   CALL      'XSRDA8'
043900140305     C                   PARM                    WLBDAT
044000140305    1C     G02ERR        IFEQ      '1'
044100140305     c                   eval      v1dmsg = 'Data errata'
044200140305     C                   SETON                                        3328
044300140305     C                   leavesr
044400140305     c                   endif
044500140305     c                   move      g02inv        wvdda             8 0
044600140305     c                   move      g02inv        vddaa             8
044700140616     c                   move      g02dat        vdda
044800140305    1C                   IF        g02inv <= DATEU8 and
044900140305     C                             §ogdda <>  Vddaa
045000140305     c                   eval      v1dmsg = $msg(4)
045100140305     C                   SETON                                        3328
045200140305     C                   leavesr
045300140305    1C                   ENDIF
045400140905     c                   else
045500140905     c                   clear                   wvdda
045600140305     c                   endif
045700140613      *test per vedere se la filiale è gestita da altra
045800140613     c                   if        not *in40
045900140613     c                   Clear                   fnlv55ds
046000140613     c                   Eval      d55tpt = '6'
046100140613     c                   Move      vfil          d55lin
046200140807     c                   if        wvdda > 0
046300140613     c                   Eval      d55drf = wvdda
046400140807     c                   else
046500140807     c                   Eval      d55drf = *date
046600140807     c                   endif
046700140613     c                   Call      'FNLV55R'
046800140613     c                   Parm                    Fnlv55ds
046900140807     c                   if        d55lin <> d55tfa
047000140807     c                   if        not wforza
047100140613     c                   eval      v1dmsg = 'La filiale alla data risulta in £6'
047200140807     c                             + 'Enter forza!'
047300140613     C                   SETON                                        4028
047400140807     c                   eval      wforza = *on
047500140613     c                   leavesr
047600140807     c                   EndIf
047700140613     c                   EndIf
047800140613     c                   EndIf
047900140305      * Procedura GIRI su ORM
048000140305     c                   if        vcgio = 'S' and vtlls = ' '
048100140305     c                   seton                                        2890
048200140305     c                   movel     $msg(5)       v1dmsg
048300140305     c                   leavesr
048400140305     c                   endif
048500140807     c                   if        vcgio = 'S' and vddao = 0
048600140807     c                   seton                                        3290
048700140807     c                   eval      v1dmsg = 'Se attiva gestione ORM -
048800140807     c                             va inserita la data'
048900140807     c                   leavesr
049000140807     c                   endif
049100140305      * Data inizio distinta ORM automatica
049200140305     c                   if        vddao > 0
049300140305     C                   CLEAR                   WLBDAT
049400140305     C                   MOVEL     '1'           G02ERR
049500140305     C                   move      vddao         G02dat
049600140305     C                   CALL      'XSRDA8'
049700140305     C                   PARM                    WLBDAT
049800140305    1C     G02ERR        IFEQ      '1'
049900140305     c                   eval      v1dmsg = 'Data errata'
050000140305     C                   SETON                                        3228
050100140305     C                   leavesr
050200140305     c                   endif
050300140305     c                   move      g02inv        wvddao            8 0
050400140305     c                   move      g02inv        vddaoa            8
050500140616     c                   move      g02dat        vddao
050600140305    1C                   IF        g02inv <= DATEU8 and
050700140305     C                             §ogddao <>  Vddaoa
050800140305     c                   eval      v1dmsg = $msg(4)
050900140305     C                   SETON                                        3228
051000140305     C                   leavesr
051100140305    1C                   ENDIF
051200140305     c                   endif
051300140305      *
051400140305     C                   ENDSR
051500130830      *
051600080228** - $Msg -------------------------------------------------------------------*
051700080228Utente NON abilitato - Premere "Invio" per tornare a menù                      01
051800140305Se precedente valore " " si può inserire solo D                                02
051900140305Per attivare telefonate in consegna deve essere già attivo lo scambio dati     03
052000140305La data deve essere maggiore di oggi                                           04
052100140305Per attivare Gestione ORM deve essere già attivo lo scambio dati               05
052200140905Se si passa da D a S vanno attivati tutti i campi nella videata                06
052300140313** CMD   (Lunga 80)                                                            *
052400140620OVRDBF FILE(AZORG01L) TOFILE(                                                     1
052500140620OVRDBF FILE(fnarb00f) TOFILE(                                                     2
052600140620OVRDBF FILE(fnarg00f) TOFILE(                                                     3
052700140620OVRDBF FILE(fidst01l) TOFILE(                                                     4
