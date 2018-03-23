000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400161010     Ffiapd01l  if   e           k disk
000401180118     Ffiapd41l  if   e           k disk
000500161010     Faitra03l  if   e           k disk
000600090625     Faitrs01l  if   e           k disk
000700161010     Fazorg01l  if   e           k disk
000800161010     Ftntbe01l  if   e           k disk
000900161013     Ffitgt01l  if   e           k disk
001000161011     Fbiaut00f  o    e             disk
001100161013      * ?___________________________________________________________________
001200161013      *
001300161013     dparam            ds
001400161013     d datap                          8  0
001401180302     d datap2                         8  0
001402180117     D paramcel        DS
001403180117     D  Icodaut                       7s 0
001404180117     D  Ocodaut                       7s 0
001405180117     D  Onumcel                      16
001406180117     d  Oesito                        1
001500161013      *
001600020304     dkpjba          e ds
001700151116     d Tibs36Ds      e ds
001800151116     D trmz70s1ds    E DS
001900170403     D trmz70s4ds    E DS                  prefix(S4_)
002000170403     d dtpa          e ds
002100121105     d datu          e ds
002200161010     d dapdflr       e ds
002201180118     d dapd4tel      e ds
002300161010     d og150         e ds
002400070605     d dataiso         s               d   datfmt(*iso)
002500070605     d dataeur         s               d   datfmt(*eur)
002600161013     d tariffa         s              1
002700070605     D WLBDA8          DS
002800070605     D  G02DAT                 1      8  0
002900070605     D  G02INV                 9     16  0
003000070605     D  G02ERR                17     17
003100070605     D  G02TGI                18     22  0
003200161011      *
003300161011     D tibsSocI0...
003400161011     D               E DS                  QUALIFIED
003500161011     D                                     INZ
003600161011     D tibsSocO0...
003700161011     D               E DS                  QUALIFIED
003800161011     D                                     INZ
003900080502      *
004000161010     D prmRqsOpCode...
004100161010     D                 S             10A
004200161010     D prmRpyOpCode...
004300161010     D                 S             10A
004400161010     D prmRpyIdMsg...
004500161010     D                 S             10I 0
004600161010     D prmRqsFormato...
004700161010     D                 S             10A
004800161010     D prmRqsDataSize...
004900161010     D                 S             10I 0
005000161010     D prmRpyFormato...
005100161010     D                 S             10A
005200161010     D prmRpyDataSize...
005300161010     D                 S             10I 0
005400161013      * ?___________________________________________________________________
005500020304      *
005600161010     c                   clear                   exfilpdr          3 0
005700161010     c     'A'           setll     fiapd01l
005800161010     c                   do        *hival
005900161010     c     'A'           reade     fiapd01l
006000161010     c                   if        %eof(fiapd01l)
006100050805     c                   leave
006200050623     c                   endif
006300161010     c                   if        apdatb <> *blank
006400161010     c                   iter
006500161010     c                   endif
006501180117      * ricerca cellulare autista per nuovo pda
006502180117     c                   clear                   celaut           16
006503180117     c                   clear                   celflg            1
006504180118     C     Kapd4         CHAIN     fiapd41L
006505180118     c                   clear                   dapd4tel
006506180118     c                   if        %found(fiapd41l)
006507180118     c                   movel     apd4uni       dapd4tel
006508180118     c                   if        §apd4cel <> *blank
006509180118     c                   movel     §apd4cel      celaut
006510180118     c                   move      'S'           celflg
006512180118     c                   else
006513180118     c                   exsr      CercaCel
006514180118     c                   if        onumcel <> *blank and Oesito = '1'
006515180118     c                   movel     onumcel       celaut
006516180118     c                   move      'N'           celflg
006517180118     c                   endif
006518180118     c                   endif
006519180118     c                   endif
006520180117      *
006600161010     C                   movel     apdpdr        filpdr            3 0
006700161010     c                   if        filpdr <> exfilpdr
006800161010     c                   move      filpdr        exfilpdr
006900161010     c     exfilpdr      chain     azorg01l
007000161010     C                   MOVEL     ORGdf0        og150
007100161010     c                   endif
007200161010     c                   clear                   biaut000
007300161011     c                   movel     apdflr        dapdflr
007400161010     c                   move      ApdPDR        AUTPDR
007500161010     c                   move      ApdRSC        AUTRSC
007600161010     c                   move      ApdRSF        AUTRSF
007601180117     c                   move      celaut        AUTnumt
007602180117     c                   move      celflg        AUTtelp
007700161013      *prova reperire il fornitore dalla tariffa
007800161013     c                   clear                   tariffa
007900170403     c                   clear                   savtgtfil         3 0
008000161013     c                   exsr      repdatariffa
008100161013     c                   if        tariffa = 'S'
008200161013     c                   move      tgtsoc        AUTCSF
008300161013     c                   move      tgtcdf        AUTKSC
008400170403     c                   move      tgtfil        savtgtfil         3 0
008500161013     c                   else
008600161010     c                   move      ApdCSF        AUTCSF
008700161013     c                   move      ApdKSC        AUTKSC
008800161013     c                   endif
008900161010      *decodifica società operativa
009000161011     c                   exsr      repsocieta
009100170403      **
009200170403      *decodifica il Fornitore per prenderne la P.IVA
009300170403     c                   clear                   savSOC_PIVA      16
009400170403     c                   exsr      decod_P_iva
009500170403      **
009600161014     c                   if        apdpdd = *blank
009700161010     c                   move      trasocgd      AUTCSFD
009800161014     c                   endif
009900170403      **
010000161010     c                   move      apdPKL        AUTPKL
010100161010     c                   move      apdVLU        AUTVLU
010200161010      *massa complessiva
010300161011     c                   if        %subst(apdflr:9:1) <> *blank
010400161010     c                   if        §apdmcp > 0
010500161010     c                   move      §apdMCP       AUTMCP
010600161010     c                   move      'MAS'         tbecod
010700161010     c                   movel     §apdmcp       tbeke1
010800161010     c     ktbe          setgt     tntbe01l
010900161010     c                   readp     tntbe01l
011000161010     c                   if        not%eof(tntbe01l)
011100161010     c                   movel     tbeuni        autmcpd
011200161010     c                   endif
011300161010     c                   endif
011400161011     c                   endif
011500161010      * km autista
011600161010     c                   if        §apdkm > '000'
011700161010     c                   move      §apdKM        AUTKM
011800161010     c                   endif
011900161028      *escluso da fatturazione
012000161013     c                   if        apdpdd <> *blank
012100161010     c                   move      ApdPDD        AUTPDD
012200161013     C                   ELSE
012300161013     C                   MOVE      'N'           AUTPDD
012400161013     C                   ENDIF
012500161028      *attività speciali
012600161028     c                   if        §APDSPEC <> *blank
012700161028     c                   move      §APDSPEC      AUTasp
012800161028     C                   ELSE
012900161028     C                   MOVE      'N'           AUTasp
013000161028     C                   ENDIF
013100161028      *bancali servizio
013200161013     c                   if        apdptp <> *blank
013300161010     c                   move      ApdPTP        AUTPTP
013400161013     c                   endif
013500161010      * utilizzo pda
013600161011     c                   if        §apdtstpda = 'E' or §apdtstpda = 'C'
013700161010     c                   move      'T'           AUTCON
013800161013     c                   eval      autcond = 'TEST PDA CONSEGNE'
013900161010     c                   else
014000161010     c                   if        §apdpda = 'E' or §apdpda = 'C'
014100161010     c                   move      'S'           AUTCON
014200161013     c                   eval      autcond = 'PDA CONSEGNE'
014300161010     c                   else
014400161010     c                   move      'N'           AUTCON
014500161013     c                   eval      autcond = 'NO PDA CONSEGNE'
014600161010     c                   endif
014700161010     c                   endif
014800161011     c                   if        §apdtstpda = 'E' or §apdtstpda = 'O'
014900161011     c                   move      'T'           AUTrit
015000161013     c                   eval      autritd = 'TEST PDA RITIRI'
015100161010     c                   else
015200161010     c                   if        §apdpda = 'E' or §apdpda = 'O'
015300161010     c                   move      'S'           AUTRIT
015400161013     c                   eval      autritd = 'PDA RITIRI'
015500161010     c                   else
015600161011     c                   move      'N'           AUTrit
015700161013     c                   eval      autritd = 'NO PDA RITIRI'
015800161010     c                   endif
015900161010     c                   endif
016000161010      *
016100161010     c                   if        §ogturno = 'S'
016200161011     c                   move      §ApdTURno     AUTTUR
016300161010     c                   select
016400161011     c                   when      §apdturno = *blank
016500161014     c                   eval      auttur  = '1'
016600161014     c                   eval      autTURD = 'PRIMO TURNO'
016700161011     c                   when      §apdturno = '1'
016800161013     c                   eval      autTURD = 'PRIMO TURNO'
016900161011     c                   when      §apdturno = '2'
017000161013     c                   eval      autTURD = 'SECONDO TURNO'
017100161010     c                   endsl
017200161010     C                   ELSE
017300161013     c                   eval      autTURD = 'TURNO UNICO'
017400161014     c                   eval      auttur  = 'U'
017500161010     C                   ENDIF
017600161010     c                   move      §apdTPM       AUTTPM
017700161010      * parcel messaggerie
017800161011     c                   if        §apdtpm = 'M'
017900161011     c                   movel     'MESSAGGERIA' AUTTPMD
018000161010     c                   else
018100161013     c                   move      'P'           AUTTPM
018200161011     c                   movel     'PARCEL'      AUTTPMD
018300161010     c                   endif
018400161013     C                   IF        APDSNU <> *BLANK
018500161013     c                   move      APDSNU        AUTSNU
018600161013     C                   ELSE
018700161013     C                   MOVE      'N'           AUTSNU
018800161013     C                   ENDIF
018900161013     c                   if        §apdGCD > '00'
019000161013     c                   move      §APDGCD       AUTGCD
019100161013     C                   ENDIF
019200161010     c                   move      §apdmezzo     AUTMEZ
019300161010      * tipo mezzo
019400161010     c                   if        §apdmezzo = 'B'
019500161010     c                   movel(p)  'BARCA'       AUTmezd
019600161010     c                   else
019700161013     c                   move      'A'           AUTMEZ
019800161010     c                   movel     'AUTOMEZZO'   AUTmezd
019900161010     c                   endif
020000161011     c                   if        §apdats <> *blank
020100161010     c                   move      §apdats       AUTTER
020200161010     c                   move      'ATS'         tbecod
020300161010     c                   movel     §apdats       tbeke1
020400161010     c     ktbe          setgt     tntbe01l
020500161010     c                   readp     tntbe01l
020600161010     c                   if        not%eof(tntbe01l)
020700161010     c                   movel     tbeuni        autterd
020800161010     c                   endif
020900161010     c                   endif
021000161129     c                   if        §apdaat <> *blank
021100161129     c                   move      §apdaat       AUTAAT
021200161129     c                   move      'AAT'         tbecod
021300161129     c                   movel     §apdaat       tbeke1
021400161129     c     ktbe          setgt     tntbe01l
021500161129     c                   readp     tntbe01l
021600161129     c                   if        not%eof(tntbe01l)
021700161129     c                   movel     tbeuni        autaatd
021800161129     c                   endif
021900161129     c                   endif
022000161010      * campi da aitra/aitrs  accreditamenti autisti e società
022100161010     c                   exsr      elabora
022200161010     c                   write     biaut000
022300050805     c                   enddo
022400091217      *
022500161010     ** Chiudo il programma.x dati societari
022600161010     C                   CALL      'TIBSSOCR'
022700161010     C                   PARM      'FINALIZE'    prmRqsOpCode
022800161010     C                   PARM                    prmRpyOpCode
022900161010     C                   PARM                    prmRpyIdMsg
023000020308     c                   seton                                        lr
023001180117     c*_____________________________________________________________
023002180117     c     CercaCel      begsr
023003180117     c*_____________________________________________________________
023004180118     c                   movel     kpjbu         kpjbusav        256
023005180117     c                   clear                   paramcel
023006180118     c                   z-add     apdpdr        icodaut
023007180117     c                   movel     paramcel      kpjbu
023008180117     c                   call      'TRULCFAUR'
023009180117     c                   parm                    kpjba
023010180117     c                   movel     kpjbu         paramcel
023011180117     c                   movel     kpjbusav      kpjbu
023012180117     c                   endsr
023100990113     c*-------------------------------------------------
023200161010     c     elabora       begsr
023300990113     c*-------------------------------------------------
023400161013     c                   move      'N'           autacr
023500170403     c                   clear                   traNRC
023600170331      **
023700170331      **  o trovo un record con DATA FINE = 0  oppure
023800170331      **   l'AUT è stato disaccreditato DFI >0
023900161010     c     apdpdr        chain     aitra03l
024000161010     c                   if        %found(aitra03l)
024100170331      **
024200170331      *   se disaccreditato prendo l'ultimo accreditamento avuto
024300170331      *   oppure se Non ha scadenza ma la data è antecedente alla Data inizio accr.
024400161010     c                   if        tradfi > 0
024500170331     c                               or
024600170331     c                             tradfi=0 and datap < tradin
024700170330      **
024800161010     c     apdpdr        setgt     aitra03l
024900161014     c                   do        *hival
025000161010     c     apdpdr        readpe    aitra03l
025100170331      **
025200170331      **   se Fine File Esce
025300170331     c                   if        %eof(aitra03l)
025400170331     c                   leave
025500161014     c                   endif
025600170331      **
025700170331      **  Se è entro data accred.e disaccr. oppure è oltre la data disaccr.
025800161014     c                   if        datap >= tradin and datap <= tradfi
025900170329     c                               or
026000170329     c                             datap > tradfi and tradfi > 0
026100170331      **
026200170331     c                   if        datap >= tradin and datap <= tradfi
026300170331     c                   move      'S'           autacr
026400170331     c                   endif
026500170331      **
026600161014     c                   leave
026700170403      **
026800161014     c                   endif
026900161014     c                   enddo
027000161014      *dfi = 0
027100161014      * se la data disaccreditamento è uguale a 0 verifico che la data inizio non sia
027200161014      * successiva a quella in esame nel caso innesco il giro come se fosse disaccreditato
027300161014     c                   else
027400170331      *
027500170331      ** se accreditato e la data di accreditamento è inferiore
027600170330     c                   move      'S'           autacr
027700161014     c                   endif
027800170331      *
027900170331      * ___DECODifica i campi di AITRA
028000170331     c                   exsr      DECOD_AITRA
028100170331      *
028200170331     c                   ENDIF
028300170331      *
028400170331     c                   endsr
028500151116      * ?___________________________________________________________________
028600170331     C     DECOD_AITRA   BegSR
028700170331      * ?___________________________________________________________________
028800170403      *
028900170403      * PER DECODIFICARE, però,
029000170403      *    Controlla l'uguaglianza della PIVA di AITRA rispetto al contratto
029100170403      **    Altrimenti non deve decodificare nulla ed impostare a "N" il flag di
029200170403      **    accreditato.
029300170403     c                   if        tranrc <> 0
029400170403     c*
029500170403     c     tranrc        chain     aitrs01l
029600170403     c                   if        %found(aitrs01l)
029700170404     c*
029800170404     c                   movel     trsiva        autiva
029900170404     c                   exsr      RagSocCont
030000170404     c                   movel     ragsocksc     AUTKSCD
030100170404     c                   endif
030200170403     c*
030300170403     c* Se la P.IVA è differente da quella salvata impossta "N" ed esce dalle
030400170403     c*  decodifiche
030500170403     c                   if        trsiva <> savSOC_PIVA
030600170403     c                   move      'N'           autACR
030700170403     c                   LEAVESR
030800170403     c                   end
030900170403     c*
031000170403     c                   end
031100170403      **
031200170331      *se ho già trovato una tariffa che mi diceva che era accreditato alla data
031300170331      *imposto quello come dato di accreditamento
031400170331     c                   clear                   autRISd
031500170331      *
031600170331     C                   IF        TRARIS <> *BLANK
031700170331     c                   move      traRIS        AUTRIS
031800170331     c                   move      'TPA'         tbecod
031900170331     c                   movel(p)  traRIS        tbeke1
032000170331     c     ktbe          chain     tntbe01l
032100170331     c                   if        %found(tntbe01l)
032200170331     c                   movel     tbeuni        dTPA
032300170331     c                   movel     §tpaDES       autRISd
032400170331     c                   endif
032500170331     C                   ELSE
032600170331     c                   move      'N'           AUTRIS
032700170331     C                   ENDIF
032800170403      *
032900170331     C                   IF        TRACOR <> *BLANK
033000170331     c                   move      traCOR        AUTCOR
033100170331     C                   ELSE
033200170331     c                   move      'N'           AUTCOR
033300170331     C                   ENDIF
033400170403      *
033500170331     c                   move      traCf         AUTCDF
033600170331     c                   move      traTAA        AUTTAA
033700171117     c                   move      trsFIL        AUTFIL
033800170331     c                   move      traNRC        AUTNRC
033900170331     c                   move      traDIN        AUTDIN
034000170331     c                   move      traDFI        AUTDFI
034100170403      *
034200170331     C                   IF        TRASPI <> *BLANK
034300170331     c                   move      traSPI        AUTSPI
034400170331     C                   ELSE
034500170331     c                   move      'N'           AUTSPI
034600170331     C                   ENDIF
034700170403      *
034800170331     c                   move      traFF2        AUTFF2
034900170403      *
035000170331     c                   if        traff2  <> *blank
035100170331     c                   move      'ALI'         tbecod
035200170331     c                   movel(p)  traff2        tbeke1
035300170331     c     ktbe          chain     tntbe01l
035400170331     c                   if        %found(tntbe01l)
035500170331     c                   movel     tbeuni        autff2d
035600170331     c                   endif
035700170331     c                   endif
035800170403      *
035900170331     c                   move      traFUR        AUTFUR
036000170331     c                   if        trafur  <> *blank
036100170331     c                   move      'ALL'         tbecod
036200170331     c                   movel(p)  trafur        tbeke1
036300170331     c     ktbe          chain     tntbe01l
036400170331     c                   if        %found(tntbe01l)
036500170331     c                   movel     tbeuni        autfurd
036600170331     c                   endif
036700170331     c                   endif
036800170403      *
036900170331     c                   move      traDIPa       AUTDIP
037000170331     c                   if        tradipa = 'A'
037100170331     c                   eval      AUTDIPD = 'AUTONOMO'
037200170627     C                   ELSEif    tradipa = 'D'
037300170331     c                   eval      AUTDIPD = 'DIPENDENTE'
037400170627     C                   ELSEif    tradipa = 'I'
037500170627     c                   eval      AUTDIPD = 'INTERINALE'
037600170331     C                   ENDIF
037700170403      *
037701170728     C                   CLEAR                   AUTATPD
037800170627     c                   move      traATP        AUTATP
037801170728     c                   if        autatp = 'S'
037802170728     c                   eval      autatpd = 'PICCO LAVORATIVO'
037804170728     C                   ENDIF
037900170627      *
038000170331     c                   move      traOREL       AUTORE
038100170331     c                   move      traITaest     AUTITES
038200170331      *
038300170331     c                   select
038400170331     c                   when      traitaest = 'I'
038500170331     c                   eval      AUTITESd = 'NAZIONALITÀ ITALIANA'
038600170331     c                   when      traitaest = 'E'
038700170331     c                   eval      AUTITESd = 'NAZIONALITÀ ESTERA'
038800170331     c                   other
038900170331     c                   clear                   autitesd
039000170331     c                   endsl
039100170331      *
039200170331     c                   move      traLIQ        AUTLIQ
039300170331     c                   if        traLIQ  <> *blank
039400170331     c                   move      'LIQ'         tbecod
039500170331     c                   movel(p)  traLIQ        tbeke1
039600170331     c     ktbe          chain     tntbe01l
039700170331     c                   if        %found(tntbe01l)
039800170331     c                   movel     tbeuni        autliqd
039900170331     c                   endif
040000170331     c                   endif
040100170403      *
040200170331     c                   move      tracnl        AUTCNL
040300170331     c                   if        tracnl  <> *blank
040400170331     c                   move      'CNL'         tbecod
040500170331     c                   movel(p)  tracnl        tbeke1
040600170331     c     ktbe          chain     tntbe01l
040700170331     c                   if        %found(tntbe01l)
040800170331     c                   movel     tbeuni        autcnld
040900170331     c                   endif
041000170331     c                   endif
041100170403      *
041200170331     c                   move      traqua        AUTQUA
041300170331     c                   if        traqua  <> *blank
041400170331     c                   move      'QUA'         tbecod
041500170331     c                   movel(p)  traqua        tbeke1
041600170331     c     ktbe          chain     tntbe01l
041700170331     c                   if        %found(tntbe01l)
041800170331     c                   movel     tbeuni        autquad
041900170331     c                   endif
042000170331     c                   endif
042100170331      *
042200170331     c                   endsr
042300170331      * ?___________________________________________________________________
042400151116     C     RagSocCont    BegSR
042500151116      * ?___________________________________________________________________
042600161010
042700151116     C                   clear                   trmz70s1ds
042800161010     C                   movel(p)  TRsiva        PartitaIVA                     Input
042900151116     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
043000161011     C                   movel(p)  autcsf        Societa                        Input/Output
043100161010     C                   movel(p)  exfilpdr      Unita                          Input/Output
043200151116      *
043300151116     c                   call      'TRMZ70SR1'
043400151116     C                   parm                    trmz70s1ds
043500151116     c*
043600151116     C                   ENDSR
043700161013      * ?___________________________________________________________________
043800161013     C     Repdatariffa  BegSR
043900161013      * ?___________________________________________________________________
044000161013
044100161013     C     apdpdr        setll     fitgt01l
044200161013     C                   do        *hival
044300161013     C     apdpdr        reade     fitgt01l
044400161013     c                   if        %eof(fitgt01l)
044500161013     c                   leave
044600161013     c                   endif
044700161013     c                   if        datap >= tgtddt and datap <= tgtdst
044800161013     c                             and tgtdts > 0
044900161013     c                   move      'S'           tariffa
045000161013     c                   leavesr
045100161013     c                   endif
045200161013     c                   enddo
045300161013     c*
045400161013     C                   ENDSR
045500161010     c*-------------------------------------------------
045600161011     c     repsocieta    begsr
045700161010     c*-------------------------------------------------
045800161010     c                   clear                   trasocgd         44
045900161010     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
046000161010     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
046100161010     C                   RESET                   tibsSocI0
046200170403     C                   EVAL      tibsSocI0.idSocieta = AUTcsf
046300161010     C
046400161010     C                   CALL      'TIBSSOCR'
046500161010     C                   PARM      'GETANAGRAF'  prmRqsOpCode
046600161010     C                   PARM      *BLANK        prmRpyOpCode
046700161010     C                   PARM      *ZERO         prmRpyIdMsg
046800161010     C                   PARM      'TIBSSOCI0'   prmRqsFormato
046900161010     C                   PARM                    tibsSocI0
047000161010     C                   PARM                    prmRqsDataSize
047100161010     C                   PARM      'TIBSSOCO0'   prmRpyFormato
047200161010     C                   PARM                    tibsSocO0
047300161010     C                   PARM                    prmRpyDataSize
047400161010     c                   if         PRMRPYIDMSG >= 0 and
047500161010     c                              TIBSSOCO0.IDSOCIETA <> *blank
047600161010     c                   eval      trasocgd = tibsSocO0.RAGSOCIALE
047700161010     c                   else
047800161010     c                   eval      trasocgd = 'Società Errata'
047900161010     c                   end
048000161010     C                   endsr
048100011026     c*-------------------------------------------------
048200011026     c     *inzsr        begsr
048300011026     c*-------------------------------------------------
048400020304     c     *entry        plist
048500020304     c                   parm                    kpjba
048600050801
048700020218     C                   time                    w0120            14 0
048800020218     C                   move      w0120         wdat              8 0
048900161013     c                   move      wdat          dataeur
049000161013     c                   move      dataeur       dataiso
049100161013     c                   move      dataiso       wdat
049200161013     c                   if        kpjbu <> *blank
049300161013     c                   movel     kpjbu         param
049400161013     c                   else
049500161013     c                   move      wdat          param
049600161013     c                   endif
049700161013      *
049800161013     c     ktbe          klist
049900161013     c                   kfld                    tbecod
050000161013     c                   kfld                    tbeke1
050100161013     c
050101180117     C     Kapd4         KLIST
050102180117     C                   KFLD                    tipo              1
050103180117     C                   KFLD                    apdpdr
050104180117     C                   KFLD                    ktrd
050105180117     c                   move      'A'           tipo
050106180117     c                   move      'TEL'         ktrd              3
050200161013
050300030630      *profilo di filiale estraggo solo quelli relativi al P.O.
050400030630     C                   CLEAR                   Tibs36Ds
050500030630     C                   EVAL      I36ute = knmus
050600030630     C                   CALL      'TIBS36R'
050700030630     C                   PARM                    tibs36ds
050800050802
050900161010     ** Inizializzo il programma. per dati societari
051000161010     C                   CALL      'TIBSSOCR'
051100161010     C                   PARM      'INIT'        prmRqsOpCode
051200161010     C                   PARM                    prmRpyOpCode
051300161010     C                   PARM                    prmRpyIdMsg
051400161010
051500011026     c                   endsr
051600170403     c*--------------------------------------------------------------
051700170403     c* tramite Società e Unità decodifica P.IVA su PROJ
051800170403     c*--------------------------------------------------------------
051900170403     C     Decod_P_IVA   BegSR
052000170403      **
052100170403      **  Routine x Reperire dati PIVA:
052200170403      **    La routine in base alla sottonatura può funzionare
052300170403      **     x F=Fornitore/C=Cliente
052400170403     C                   clear                   trmz70s4ds                     Input
052500170403     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
052600170403     C                   move      autKSC        s4_keyKSC                      Input
052700170403     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
052800170403     C                   movel(p)  autCSF        s4_Societa                     Input/Output
052900170403     c                   if        savtgtfil > 0
053000170403     C                   movel(p)  savtgtfil     s4_Unita                       Input/Output
053100170403     c                   end
053200170403     c                   call      'TRMZ70SR4'
053300170403     c                   parm                    trmz70s4ds
053400170403      *
053500170403      *  se torna con l'errore riprova e aveva passato l'unità
053600170403      *   riprova passandogli l'unità vuota
053700170403     c                   if        s4_errore <> '0'
053800170403     C                   clear                   trmz70s4ds                     Input
053900170403     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
054000170403     C                   move      autKSC        s4_keyKSC                      Input
054100170403     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
054200170403     C                   movel(p)  autCSF        s4_Societa                     Input/Output
054300170403     c                   call      'TRMZ70SR4'
054400170403     c                   parm                    trmz70s4ds
054500170403     c                   end
054600170403      *
054700170403     c                   eval      savSOC_PIVA = s4_PARTITAIVA
054800170403      *
054900170403     C                   ENDSR
055000170403     C/EJECT
055100170403     c**********************************************************************
