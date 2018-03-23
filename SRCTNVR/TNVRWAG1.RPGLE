000100031205      *------------------------------------------------------------------------*
000200071116      * Nuovo file clienti   potenziali di work con dati D&B
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600080228     FNSCATECO  iF   E             DISK    rename(nscateco:nscateco00)
000700080205     fTncpo01l  uf a e           k Disk
000800080219     fTncps11l  if   e           k Disk
000900080212     fTncpo35l  if   e           k Disk    rename(tncpo000:tncpo035)
001000080219     f                                     prefix(X)
001100071123     fTntbe01l  if   e           k Disk
001200071123     fTabel00f  if   e           k Disk
001300080211     ftnvis05l  if   e           k Disk
001400080211     fcnaco16l  uf   e           k Disk    rename(cnaco000:cnaco016)
001500080212     fcnaco00f  uf   e           k Disk    prefix(x)
001600080219     fTFACO16l  uf   e           k Disk    rename(cnaco000:tfaco016)
001700080219     ftfaco00f  uf   e           k Disk    prefix(x)
001800080211     f                                     rename(cnaco000:tfaco000)
001900080211     fcnclp00f  if   e           k Disk
002000080211     ftfclp00f  if   e           k Disk    rename(cnclp000:tfclp000)
002100080211     fcnind00f  if   e           k Disk
002200080220     ftncpo05l  uf   e           k Disk    rename(tncpo000:tncpo005)
002300080220     ftncpo06l  uf   e           k Disk    rename(tncpo000:tncpo006)
002400080211     ftfind00f  if   e           k Disk    rename(cnind000:tfind000)
002500080211     f
002600080207     FWFDEb1mat iF   E           k DISK
002700071116     FWFDEb1fil iF   E           K DISK
002800071116     FWFDEb1sed iF   E           K DISK
002900031205
003000031205      *------------------------------------------------------------------------*
003100031205      *   C A M P I   D I   L A V O R O
003200031205      *------------------------------------------------------------------------*
003300080226     d PRVALT          s              2    dim(15) CTDATA PERRCD(15)
003400080226     D
003500080226     d ksc             s              7  0 dim(500)
003600080219     d scf             s              7  0 dim(500)
003700080219     d cmm             s              7  0 dim(500)
003800080219     d dus             s              6  0 dim(500)
003900080221     d dvi             s              8  0 dim(500)
004000080219     d blo             s              1    dim(500)
004100080220     d pksc            s              7  0 dim(500)
004200080220     d pscf            s              7  0 dim(500)
004300080220     d pdus            s              6  0 dim(500)
004400080220     d pblo            s              1    dim(500)
004500080211     d
004600080211     d TipoChiamata    s              1
004700080219     d Aggiornato      s              1
004800080211     d Wtipo           s              1
004900080212     d WAggio          s              1
005000080211     d Trova           s              1
005100080219     d Finelet         s              1
005200080213     d Indx            s              3  0
005300080207     d Kcpo            s             11  0
005400080211     d Kksc            s              7  0
005500080219     d Kscf            s              7  0
005600080212     d Kcmm            s              7
005700080211     d Unicoscf        s              7  0
005800080220     d Unicoblo        s              1
005900071128     d wleft           s              2  0
006000080212     d kk              s              5  0
006100080219     d yy              s              5  0
006200080212     d savkk           s              5  0
006300080219     d savyy           s              5  0
006400080220     d wfile           s              2
006500080207     d W036a           s             36
006600080211     d DeBloc          s             35
006700080211     d DeBfraz         s             35
006800080220     d Deblprv         s              2
006900080220     d Debfprv         s              2
007000080211     d WindLoc         s             35
007100080219     d Windprv         s              2
007200080213     d savcap          s                   like(cpocap)
007300080213     d savprv          s                   like(cpoprv)
007400080213     d savcpo          s                   like(cpocpo)
007500080219     d wdus            s              8  0
007600080219     d savdus          s              6  0
007700080221     d savdvi          s              8  0
007800080212     d savsct          s                   like(cposct)
007900080212     d Kpiv            s                   like(indiva)
008000080211     d kut             s                   like(acokut) inz(1)
008100080211     d kcc             s                   like(acokcc) inz(151)
008200071122     d Datasys         s               d   inz(*sys) datfmt(*iso)
008300071128     d Numeri          C                   '0123456789'
008400031205
008500031205      *------------------------------------------------------------------------*
008600031205      *   D S   I N T E R N E / E S T E R N E
008700031205      *------------------------------------------------------------------------*
008800031205     d Kpjba         e ds
008900071122     d fnlv13ds      e ds
009000071122     d tisi95ds      e ds
009100071123     d DCMA          e ds
009200071122     d
009300071121     D DSIndirizzo     DS
009400071121     d  I_Rag                        90
009500071121     d  I_via                        64
009600071121     d  I_fraz                       64
009700071121     d  I_cit                        30
009800071121     d  I_regione                    14
009900071121     d  I_cap                         5
010000071121     d  I_prv                         2
010100071116     d
010200071116     D DSFil           DS
010300071121     d  F_Indirizzo                 269
010400071116     d
010500071116     D DSSed           DS
010600071121     d  S_INDIRIZZO                 269
010700071116     d  S_fgiu                       13
010800071116     d  S_piv                        11
010900071116     d  S_cdf                        16
011000071116     d  S_tel                        14
011100071116     d  S_sic                         4
011200071116     d  S_ateco                       7
011300071128     d  S_annfat                      4
011400071128     d  S_totfat                     11
011500071116     d  S_dicfat                      1
011600071116     d  S_stifat                      1
011700071128     d  S_export                      3
011800071116     d  S_paduns                      9
011900071122     d  S_parag                      64
012000071122     d  S_pavia                      64
012100071122     d  S_pacit                      30
012200071122     d  S_pacap                       9
012300071122     d  S_paprv                       2
012400071122     d  S_panaz                       3
012500071122      *------------------------------------------------------------------------*
012600071122      *   P R O T O T I P I
012700071122      *------------------------------------------------------------------------*
012800071122     D FNLV13R         pr                  extpgm('FNLV13R')
012900071122     D  kpjba                              likeds(kpjba)
013000071122     D  FNLV13DS                           likeds(FNLV13DS)
013100071122     D  TISI95DS                           likeds(TISI95DS)
013200071122     D
013300031205      *------------------------------------------------------------------------*
013400080213     C     *ENTRY        PLIST
013500080213     C                   PARM                    SICLI             1
013600080213     C                   PARM                    SICLIPROV         1
013700080213     C                   PARM                    SIDEBFIL          1
013800080213     C                   PARM                    SIDEBSED          1
013900080213     C                   PARM                    SICLIPIV          1
014000080213     C                   PARM                    SICLIAGG          1
014100080212      /free
014200080212
014300080212       // 1a) Aggiorno potenziali con anagrafica clienti               vv
014400080212       //       --> indirizzo completo, commerciale , cat.merceologica p.o.
014500080211
014600080213       IF SICLI='S'    ;
014700080211       EXSR    ELAB_CLIENTI ;
014800080213       ENDIF    ;
014900080211
015000080212       // 1b) Aggiorno potenziali con anagrafica clienti provvisoria   vv
015100080212       //     non legata a clienti esistenti
015200080212       //       --> indirizzo completo, commerciale , cat.merceologica p.o.
015300080211
015400080213       IF SICLIPROV='S'    ;
015500080211       EXSR    ELAB_CLIPROV ;
015600080213       ENDIF   ;
015700080208
015800080207
015900080212       // 2) Aggiorno potenziali con clienti D&B Filiale matching      vv
016000071116
016100080213       IF SIDEBFIL ='S'    ;
016200080211       EXSR    ELAB_DeBFIL  ;
016300080213       ENDIF   ;
016400071122
016500080212       // 3) Aggiorno potenziali con clienti D&B SEde    matching      vv
016600080212
016700080213       IF SIDEBSED ='S'    ;
016800080212       EXSR    ELAB_DeBSED  ;
016900080213       ENDIF   ;
017000080212
017100080212       // 4) Aggiorno potenziali con file D&B Sedi per partita iva o   vv
017200080212       //    cod.fiscale. corrispondenza con provincia e località norm.
017300080211
017400080213       IF SICLIPIV ='S'    ;
017500080212       EXSR    ELAB_CLIPIV ;
017600080213       ENDIF   ;
017700080211
017800080211
017900080212       // 5) Aggiungo ai potenziali i clienti D&B SEde  mancanti       vv
018000080213
018100080213       IF SICLIAGG ='S'    ;
018200080215       EXSR    AGGIU_DeBSED  ;
018300080213       ENDIF  ;
018400080207
018500080215
018600071121       *inlr=*on         ;
018700080211       //-------------------------------------------------------------
018800080211       BEGSR   ELAB_CLIENTI  ;
018900080211       Wtipo='C'   ;
019000080211       clear    ksc ;
019100080211       clear    scf ;
019200080211       clear    dus ;
019300080219       clear    blo ;
019400080219       clear    kk  ;
019500080219       clear    yy  ;
019600080211       kcpo=1   ;
019700080211       clear savcpo ;
019800080211       setll  kcpo    cnaco16l   ;
019900080219       read(N) cnaco16l ;
0200000802111      dow  not %eof(cnaco16l);
020100080211
0202000802112      if acolib<>savcpo ;
020300080211
0204000802113      if    savcpo>0  ;
020500080212       exsr  CambioPOT       ;
020600080211       clear savcpo    ;
0207000802113      endif           ;
020800080211
020900080211       chain   acolib tncpo01l  ;
0210000802113        if not %found(tncpo01l)  ;
021100080219         chain   (kut:kcc:acoksc)   cnaco00f ;
021200080220           clear acolib    ;
021300080220           clear xacolib    ;
021400080219           clear xacodtr    ;
021500080219           clear xacoftr    ;
021600080219           update cnaco000 ;
021700080212 x3      else            ;
021800080211           savcpo=acolib ;
021900080211           clear    ksc ;
022000080220           clear    pksc;
022100080211           clear    scf ;
022200080220           clear    pscf ;
022300080211           clear    dus ;
022400080220           clear    pdus ;
022500080219           clear    blo ;
022600080220           clear    pblo ;
022700080211           clear    kk  ;
022800080219           clear    YY  ;
022900080211 3       endif ;
023000080211 2     endif  ;
023100080211
023200080220 2a    if acolib >0 ;
023300080211       kk=kk+1  ;
023400080211       ksc(KK)=acoksc ;
023500080219       blo(KK)=acoabl ;
023600080211       chain  (acokut:acokcc:acoksc)  cnclp00f ;
023700080211 2     if %found(cnclp00f) ;
023800080211       scf(KK)=clpscf ;
023900080220 3       if  clpdus>0     ;
024000080220 3       if  clpdus>100000;
024100080218          wdus=19000000 ;
024200080218         else ;
024300080218          wdus=20000000 ;
024400080218 3       endif ;
024500080218       wdus=wdus +clpdus ;
024600080220       endif    ;
024700080219       dus(kk)=%int(%subst(%editc(wdus: 'X'):1:6))  ;
024800080211 2     endif  ;
024900080219
025000080219       chain  (acokut:acokcc:acoksc)  cnind00f ;
025100080219       // se stessa provincia del cpo memorizzo a parte
025200080226       if indprv<>cpoprv ;
025300080226         INDX=%LOOKUP(INDPRV:PRVALT) ;
025400080226         IF INDx>0          ;
025500080226          EXSR CALLTISIIND  ;
025600080226         ELSE              ;
025700080226          o95err='E'        ;
025800080226         endif             ;
025900080226
026000080226       endif             ;
026100080226
026200080226       if (indprv=cpoprv) or (o95err=' ' and
026300080226                             (cpoprv=o95prv or cpoprv=o95pra));
026400080219       yy=yy+1 ;
026500080219       pksc(YY)=ksc(KK)  ;
026600080219       pscf(YY)=scf(KK)  ;
026700080219       pdus(YY)=dus(KK)  ;
026800080219       pblo(YY)=blo(KK)  ;
026900080219       endif   ;
027000080220 2a    endif   ;
027100080211
027200080219       read(N)   cnaco16l ;
027300080211 1     enddo   ;
027400080211       ENDSR   ;
027500080211       //-------------------------------------------------------------
027600080211       BEGSR   ELAB_CLIPROV ;
027700080211       Wtipo='P'   ;
027800080211       kcpo=1   ;
027900080211       clear    ksc ;
028000080211       clear    scf ;
028100080211       clear    dus ;
028200080221       clear    dvi ;
028300080211       clear    cmm ;
028400080219       clear    kk  ;
028500080211       clear savcpo ;
028600080211       setll  kcpo    tfaco16l   ;
028700080219       read(N)        tfaco16l ;
028800080211
0289000802111      dow  not %eof(tfaco16l);
029000080211       chain   acoksc   tnvis05l   ;
0291000802112      if   visksc=0   ;
029200080213
029300080213       // SE già cliente lo salto
029400080219       chain(N) acolib   cnaco16l   ;
0295000802132a     if  not %found(cnaco16l)   ;
029600080211
0297000802113        if acolib<>savcpo ;
029800080211
0299000802114           if    savcpo>0  ;
030000080211            exsr  CambioPOTProv ;
030100080211            clear savcpo    ;
0302000802114           endif           ;
030300080211
030400080211         chain   acolib tncpo01l  ;
0305000802114           if not %found(tncpo01l)  ;
030600080219         chain   (kut:kcc:acoksc)   tfaco00f ;
030700080221            clear acolib    ;
030800080219            clear xacolib    ;
030900080219            clear xacodtr    ;
031000080219            clear xacoftr    ;
031100080219            update tfaco000 ;
031200080211            else            ;
031300080211            savcpo=acolib ;
031400080211            clear    ksc ;
031500080221            clear    dvi ;
031600080212            clear    cmm ;
031700080211            clear    kk  ;
031800080211 4          endif ;
031900080211 3       endif  ;
032000080211
032100080221         if acolib>0  ;
032200080211         kk=kk+1  ;
032300080211         ksc(KK)=acoksc ;
032400080221         dvi(KK)=visdvi ;
032500080211         cmm(KK)=viscmm ;
032600080213 2a    endif  ;
032700080221
032800080221 2a    endif  ;
032900080213 2     endif  ;
033000080211
033100080219       read(N)   tfaco16l ;
033200080211 1     enddo   ;
033300080211       ENDSR   ;
033400080211       //-------------------------------------------------------------
033500080211       BEGSR   ELAB_DeBFIL  ;
033600080211       setll *loval  wfdeb1mat   ;
033700080211       read          wfdeb1mat   ;
0338000802111      dow  not %eof(wfdeb1mat);
033900080211
034000080211       clear  DSIndirizzo   ;
034100080211       clear   dsFil ;
034200080211       clear   dsSed ;
034300080211
034400080212       // Elaboro solo le cessate attività/clienti obsoleti e filiali ;
0345000802252      if (M_duns<>*blanks and m_duns<>M_duns_sed) or m_obs='O' ;
034600080211
034700080211       // Trasformo blanks in zeri  per il cod potenziale
034800080211       m_cpo = %xlate(' ':'0':m_cpo)   ;
034900080211
035000080211       kcpo=%int(M_cpo)       ;
035100080211       chain     kcpo  tncpo01l    ;
0352000802113      if     %found(tncpo01l)   ;
035300080211
035400080211       // se cessata attività imposto la O per poter alla fine verificare
035500080211       //  se è da cancellare
0356000802114          if  M_obs='O'   ;
035700080211           cpoffaz='O'   ;
035800080211x4         else   ;
035900080211
0360000802255            if m_duns<>M_duns_sed ;
036100080211
036200080211             // FILIALE
036300080211               cpoduns = M_duns   ;
036400080211               cpofsf='F'  ;
036500080211                 exsr ELABfiliale ;
0366000802115            endif       ;
0367000802114          endif    ;
036800080211
036900080211           cpodtr=%dec(datasys:*iso) ;
037000080211           update tncpo000;
037100080211
0372000802113        endif    ;
0373000802112      endif    ;
037400080211
037500080211       read wfdeb1mat;
0376000802111      enddo    ;
037700080211       ENDSR     ;
037800080212       //-------------------------------------------------------------
037900080212       BEGSR   ELAB_DeBSED  ;
038000080212       setll *loval  wfdeb1mat   ;
038100080212       read          wfdeb1mat   ;
0382000802121      dow  not %eof(wfdeb1mat);
038300080212
038400080212       clear  DSIndirizzo   ;
038500080212       clear   dsFil ;
038600080212       clear   dsSed ;
038700080212
038800080212       // Elaboro solo i clienti sede
0389000802122      if m_duns= M_duns_sed and m_duns<>*Blanks and m_obs=' ' ;
039000080212
039100080212       // Trasformo blanks in zeri  per il cod potenziale
039200080212       m_cpo = %xlate(' ':'0':m_cpo)   ;
039300080212
039400080212       kcpo=%int(M_cpo)       ;
039500080212       chain     kcpo  tncpo01l    ;
0396000802123      if     %found(tncpo01l)   ;
039700080212
039800080212             // SEDE
039900080212                 exsr ELABsede    ;
040000080212
040100080212           update tncpo000;
040200080212
0403000802123        endif    ;
0404000802122      endif    ;
040500080212
040600080212       read wfdeb1mat;
0407000802121      enddo    ;
040800080212       ENDSR     ;
040900080212       //-------------------------------------------------------------
041000080215       BEGSR   AGGIU_DeBSED ;
041100080225       eval kcpo=04601999999   ;
041200080212
041300080212        setll  *loval    Wfdeb1sed  ;
041400080212        read    wfdeb1sed   ;
041500080212
041600080212 1      dow  not  %eof(wfdeb1sed)  ;
041700080212
041800080212       clear  DSIndirizzo   ;
041900080212       clear   dsSed ;
042000080212
042100080212         chain  debs_duns   tncpo35l   ;
042200080225       // non la devo aver già aggiornata come sede
042300080225       setll   debs_duns   tncpo35l   ;
042400080225       reade   debs_duns   tncpo35l   ;
042500080225       dow    not %eof(tncpo35l) and xcpofsf<>'S'      ;
042600080225       reade   debs_duns   tncpo35l   ;
042700080225       enddo   ;
042800080212
0429000802251      if %eof(tncpo35l) or  xcpofsf<>'S'  ;
043000080212
043100080212              // Verifico che non ci sia il codice pote che attribuisco
043200080225              kcpo= kcpo+1   ;
043300080225
043400080212              clear trova   ;
043500080212              dow   trova=' '     ;
043600080212              chain   kcpo   tncpo01l ;
043700080212              if %found(tncpo01l) ;
043800080212              kcpo= kcpo+1   ;
043900080212              else   ;
044000080212              trova='S'   ;
044100080212              endif  ;
044200080212              enddo   ;
044300080212
044400080212              exsr   SCRIVInuovo ;
044500080212 2       endif    ;
044600080212
044700080212        read    wfdeb1sed   ;
044800080212 1      enddo                      ;
044900080212        ENDSR  ;
045000080212       //-------------------------------------------------------------
045100080212       BEGSR   ELAB_CLIPIV  ;
045200080212       setll  *loval    Wfdeb1sed  ;
045300080212       read    wfdeb1sed   ;
045400080212
045500080212 1     dow  not  %eof(wfdeb1sed)  ;
045600080214
045700080212
045800080212       clear  DSIndirizzo   ;
045900080212       clear   dsSed ;
046000080212       dsSed=debs_rec   ;
046100080212       DSindirizzo=s_indirizzo  ;
046200080212
046300080225 1a    if    s_piv<>*blanks or s_cdf<>*blanks   ;
046400080225
046500080212       // Normalizzo località col tisi95  ;
046600080212       clear DeBloc        ;
046700080212       clear DeBfraz       ;
046800080219       clear DeBlprv       ;
046900080219       clear DeBfprv       ;
047000080212       TipoChiamata='L'    ;
047100080212       EXSR CALLTisi   ;
047200080212
047300080212       clear kksc    ;
047400080212       waggio='S'    ;
047500080212
047600080212 2     if    s_piv<>*blanks  ;
047700080212       kpiv  =S_piv          ;
047800080219       setll  kpiv    tncpo05l        ;
047900080219       reade  (kpiv)  tncpo05l        ;
048000080225
048100080219 3     dow    not %eof(tncpo05l) and WAggio='S' ;
048200080225
048300080225 4     if   cpoduns=*blanks   ;
048400080212
048500080212       // considero solo a parità di provincia e località normalizzata
048600080220       wfile='05'       ;
048700080212       EXSR  ContrLOC   ;
048800080225 4     endif            ;
048900080225
049000080219       reade  (kpiv)   tncpo05l       ;
049100080212 3     enddo    ;
049200080212
049300080219       setll  (kpiv)   tncpo06l       ;
049400080219       reade  (kpiv)   tncpo06l       ;
049500080219 3     dow    not %eof(tncpo06l) and WAggio='S' ;
049600080212
049700080212       // considero solo a parità di provincia e località normalizzata
049800080225 4     if   cpoduns=*blanks  and cpopiv<>cpocdf ;
049900080220       wfile='06'       ;
050000080212       EXSR  ContrLOC   ;
050100080225 4     endif            ;
050200080225
050300080219       reade  (kpiv)   tncpo06l       ;
050400080212 3     enddo    ;
050500080212 2     endif                 ;
050600080212
050700080225       // Elaboro iil codice fiscale solo se pieno e <> dalla partiva i
050800080225 2     if    s_cdf<>*blanks and s_cdf<>S_piv ;
050900080212       kpiv  =S_cdf          ;
051000080219       setll  (kpiv)   tncpo05l       ;
051100080219       reade  (kpiv)   tncpo05l       ;
051200080219 3     dow    not %eof(tncpo05l) and WAggio='S' ;
051300080212
051400080212       // considero solo a parità di provincia e località normalizzata
051500080225 4     if   cpoduns=*blanks   ;
051600080220       wfile='05'       ;
051700080212       EXSR  ContrLOC   ;
051800080225       endif    ;
051900080225
052000080219       reade  (kpiv)   tncpo05l       ;
052100080212 3     enddo    ;
052200080212
052300080225       // elaboro come partita iva solo se pieni 11 caratteri   ;
052400080225 3     if %subst(S_cdf:12:5)=*blanks ;
052500080219       setll  (kpiv)   tncpo06l       ;
052600080219       reade  (kpiv)   tncpo06l       ;
052700080225 4     dow    not %eof(tncpo06l) and WAggio='S' ;
052800080212
052900080212       // considero solo a parità di provincia e località normalizzata
053000080225 5     if   cpoduns=*blanks  and cpopiv<>cpocdf ;
053100080220       wfile='06'       ;
053200080212       EXSR  ContrLOC   ;
053300080225 5     endif    ;
053400080225
053500080219       reade  (kpiv)   tncpo06l       ;
053600080225 4     enddo    ;
053700080225 3     endif                 ;
053800080225 2     endif                 ;
053900080225 1a    endif                 ;
054000080212
054100080212       read    wfdeb1sed   ;
054200080212 1     enddo    ;
054300080212       ENDSR   ;
054400080211       //-------------------------------------------------------------
054500080211       BEGSR  CambioPOTProv;
054600080219       clear  kscf    ;
054700080219
054800080211       // Se rapporto 1 a 1 cioà una unica anagrafica legata a potenz.
054900080211       //    --> la aggiorno
055000080211 1     if   kk=1  ;
055100080211         kksc=ksc(KK)      ;
055200080212         kcmm=%editc(cmm(kk): 'X')   ;
055300080211         exsr  AggioPOT  ;
055400080211 x1    else   ;
055500080211
055600080211       // se più codici legati, guardo alla data visita
055700080211       //    --> prendo la più recente
055800080221           savdvi=dvi(1)   ;
055900080211           savkk=1        ;
056000080211           kk=2   ;
056100080221 2         dow dvi(KK)> 0 ;
056200080221 3         if dvi(kk)>savdvi   ;
056300080221            savdvi=dvi(kk)  ;
056400080211            savkk=kk   ;
056500080211 3         endif     ;
056600080211           kk=kk+1   ;
056700080211 2         enddo     ;
056800080211
056900080211         kksc=ksc(savkk)   ;
057000080212         kcmm=%editc(cmm(savkk): 'X')   ;
057100080211         exsr AggioPOT   ;
057200080211 1     endif    ;
057300080211
057400080211       ENDSR   ;
057500080211       //-------------------------------------------------------------
057600080211       BEGSR  CambioPOT    ;
057700080219       clear  Aggiornato   ;
057800080211       // Se rapporto 1 a 1 cioà una unica anagrafica legata a potenz.
057900080211       //    --> la aggiorno
058000080211 1     if   kk=1  ;
058100080211         kksc=ksc(KK)      ;
058200080219         kscf=scf(KK)      ;
058300080211         exsr  AggioPOT  ;
058400080219 1     endif  ;
058500080219
058600080219       // se più codici legati, guardo alla provincia
058700080219       //    --> se unica o uno solo non bloccato aggiorno il suo cod
058800080219 1     if Aggiornato=' '  ;
058900080219 2     if   yy=1  ;
059000080219         kksc=pksc(yy)      ;
059100080219         kscf=pscf(yy)      ;
059200080219         exsr  AggioPOT  ;
059300080219 x2      else    ;
059400080219
059500080219         clear unicoblo  ;
059600080219         yy=1  ;
059700080219 3       dow   pksc(YY)>0 and unicoblo<>'9'  ;
059800080219 4       if  pblo(YY)=' '    ;
059900080219 5         if unicoblo=' ' ;
060000080219            unicoblo='N'    ;
060100080219            savyy=yy  ;
060200080219           else   ;
060300080219            unicoblo='9'   ;
060400080219 5         endif    ;
060500080219 4       endif    ;
060600080219
060700080219         yy=yy+1  ;
060800080219 3       enddo   ;
060900080219
061000080219 3       if unicoblo='N'    ;
061100080219         kksc=pksc(savyy)      ;
061200080219         kscf=pscf(savyy)      ;
061300080219         exsr  AggioPOT  ;
061400080219 3       endif    ;
061500080219 2       endif    ;
061600080219 1       endif    ;
061700080219
061800080219       // Terzo   tentativo: guardo al sottoconto int.fattura
061900080219       //    --> se è unico, uno dei codici memorizzati e non bloccato
062000080219       //        aggiorno
062100080211
062200080219 1       if Aggiornato=' '  ;
062300080219
062400080211         unicoscf=scf(1) ;
062500080211         kk=2    ;
062600080219 2       dow scf(KK)>0  and unicoscf>0 ;
062700080211           if unicoscf<>scf(kk)    ;
062800080211            clear unicoscf          ;
062900080211           endif ;
063000080211         kk=kk+1   ;
063100080219 2       enddo   ;
063200080211
063300080219 2       if unicoscf>0    ;
063400080212         indx=%lookup   (unicoscf:ksc)  ;
063500080219 3       if  indx>0    ;
063600080219         // Verifico che non sia bloccato
063700080219        chain(N) (kut:kcc:unicoscf) cnaco00f ;
063800080219 4       if %found(cnaco00f) and acoabl=' ' ;
063900080211           kksc=unicoscf    ;
064000080219           kscf=0           ;
064100080211           exsr   AggioPOT  ;
064200080219 4       endif   ;
064300080219 3       endif   ;
064400080219 2       endif   ;
064500080219 1       endif   ;
064600080212
064700080219       // Atrimenti l'ultimo che ha spedito con la stessa provincia
064800080219       //     di tncpo se trovato, altrimenti l'ultimo in assoluto
064900080219 1      if Aggiornato=' ' ;
065000080219
065100080219 2      if pksc(2)>0 ;
065200080219           savdus=pdus(1)   ;
065300080219           savyy=1        ;
065400080219           yy=2   ;
065500080219 3         dow pdus(yy)> 0 ;
065600080219 4         if pdus(yy)>savdus   ;
065700080219            savdus=pdus(yy)  ;
065800080219            savyy=yy   ;
065900080219           else      ;
066000080219
066100080219           // Se uguali tengo il primo non bloccato
066200080219 5           if pdus(yy)=savdus   and
066300080219                     pblo(savyy)<>' '  and pblo(yy)=' ' ;
066400080219                savdus=pdus(yy)  ;
066500080219                savyy=yy   ;
066600080219 5           endif     ;
066700080219 4         endif     ;
066800080219           yy=yy+1   ;
066900080219 3         enddo     ;
067000080219
067100080219         kksc=pksc(savyy)   ;
067200080219         kscf=pscf(savyy)   ;
067300080219         exsr AggioPOT   ;
067400080219  x2     else            ;
067500080219
067600080211           savdus=dus(1)   ;
067700080211           savkk=1        ;
067800080211           kk=2   ;
067900080211 3         dow dus(KK)> 0 ;
068000080211 4         if dus(kk)>savdus   ;
068100080211            savdus=dus(kk)  ;
068200080211            savkk=kk   ;
068300080219
068400080219 x4        else        ;
068500080219           // Se uguali tengo il primo non bloccato
068600080219 5           if dus(kk)=savdus   and
068700080220                  blo(savkk)<>' '  and blo(kk)=' ' ;
068800080219                savdus=dus(kk)  ;
068900080219                savkk=kk   ;
069000080219 5           endif     ;
069100080211 4         endif     ;
069200080211           kk=kk+1   ;
069300080211 3         enddo     ;
069400080219
069500080211         kksc=ksc(savkk)   ;
069600080219         kscf=scf(savkk)   ;
069700080211         exsr AggioPOT   ;
069800080211 2       endif    ;
069900080211 1     endif    ;
070000080219
070100080211       ENDSR   ;
070200080207       //-------------------------------------------------------------
070300080211       BEGSR  AggioPOT     ;
070400080219       Aggiornato ='S'   ;
070500080219
070600080211       if  wtipo='C'       ;
070700080212       chain(N) (kut:kcc:kksc)   cnaco00f ;
070800080211       chain    (kut:kcc:kksc)   cnind00f ;
070900080211       chain    (kut:kcc:kksc)   cnclp00f ;
071000080211       else    ;
071100080219       chain(N) (kut:kcc:kksc)   tfaco00f ;
071200080211       chain    (kut:kcc:kksc)   tfind00f ;
071300080211       chain    (kut:kcc:kksc)   tfclp00f ;
071400080211       endif    ;
071500080211
071600080219       // Aggiorno l'indirizzo solo se le provincie corrispondono
071700080226       if indprv<>cpoprv ;
071800080226         INDX=%LOOKUP(INDPRV:PRVALT) ;
071900080226         IF INDx>0          ;
072000080226          EXSR CALLTISIIND  ;
072100080226         ELSE              ;
072200080226          o95err='E'        ;
072300080226         endif             ;
072400080226
072500080226       endif             ;
072600080226
072700080226       if (indprv=cpoprv) or (o95err=' ' and
072800080226                             (cpoprv=o95prv or cpoprv=o95pra));
072900080219       %subst(cporst:1:1)='A' ;
073000080211       cporag=xacorag   ;
073100080211       cpovia=indvia    ;
073200080211       cpocit=indcit    ;
073300080211       cpocap=indcae    ;
073400080211       cpoprv=indprv    ;
073500080211       cponaz=indsta    ;
073600080212       if indtel<>*blanks ;
073700080211       cpotel=indtel    ;
073800080212       endif   ;
073900080212
074000080212       if indtlf<>*blanks ;
074100080211       cpofax=indtlf    ;
074200080212       endif   ;
074300080219
074400080219       // P.iva e cod fisc li aggiorno se non c'e' l'int fattura
074500080219       //         solo per clcienti
074600080219       cpocdf=indcdf    ;
074700080219       cpopiv=indiva    ;
074800080219
074900080220       if kscf>0   and kscf<>kksc     ;
075000080219         chain    (kut:kcc:kscf)   cnind00f ;
075100080219         if %found(cnind00f) ;
075200080219         cpocdf=indcdf    ;
075300080219         cpopiv=indiva    ;
075400080219         endif            ;
075500080219       endif            ;
075600080219
075700080219       endif   ;
075800080219
075900080211       if  wtipo='C'    ;
076000080212       cpoflt=%int(%subst(%editc(xacoksc: 'X'):1:3))   ;
076100080219       cpocmm=clpage    ;
076200080211       else             ;
076300080212       cpoflt=%int(%subst(kcmm:1:3))    ;
076400080219       cpocmm=%int(kcmm)    ;
076500080211       endif            ;
076600080219
076700080211       cposct=xacoitc   ;
076800080212       cpodtr=%dec(datasys:*iso) ;
076900080219
077000080219       // Se cliente bloccato cerco ultima azione commerciale
077100080219       //    da tncps
077200080219
077300080220       if wtipo='C' and  xacoabl<>' '   ;
077400080219         clear   finelet;
077500080219         setll    cpocpo   tncps11l   ;
077600080219         reade    cpocpo   tncps11l   ;
077700080219         dow    not %eof(tncps11l)  and finelet=' '   ;
077800080219           if cpoflt=%int(%subst(%editc(cpscmm: 'X'):1:3))   ;
077900080219           cpocmm=cpscmm    ;
078000080219           finelet='S'      ;
078100080219           endif            ;
078200080219
078300080219         reade    cpocpo   tncps11l   ;
078400080219         enddo            ;
078500080219
078600080219       endif            ;
078700080211
078800080212       update tncpo000     ;
078900080211       ENDSR               ;
079000080211
079100080211       //-------------------------------------------------------------
079200080211       BEGSR ContrLOC   ;
079300080219 1      if    cponaz=*blanks ;
079400080226         exsr  CALLTisICPO   ;
079500080211
079600080220       // Se corrisponde provincia e località aggiorno
079700080220 2      if    windprv=deblprv or windprv = debfprv  ;
079800080219 2a     if    Windloc=DeBloc  or Windloc=DeBfraz  ;
079900080220
080000080225       cpoduns = debs_duns   ;
080100080220       cpoFSF = 'F'      ;
080200080220       cpodtr=%dec(datasys:*iso) ;
080300080225       kcpo=cpocpo          ;
080400080220       exsr  AggioDatiSede  ;
080500080220       if   wfile='05'      ;
080600080220       update   tncpo005    ;
080700080220       else                 ;
080800080220       update   tncpo006    ;
080900080220       endif                ;
081000080211
081100080219 2a      endif    ;
081200080219 2       endif    ;
081300080211 1     endif    ;
081400080211       ENDSR    ;
081500080211       //-------------------------------------------------------------
081600080211       BEGSR  SCRIVInuovo  ;
081700080207
081800080207       dsSed= debs_rec     ;
081900080207       DsIndirizzo=S_indirizzo ;
082000080207
082100080207         clear tncpo000   ;
082200080207         cpocpo  = kcpo     ;
082300080207         cpoduns = debs_duns   ;
082400080207         cpofsf  = 'S'      ;
082500080207
082600080207         EXSR  AggioDatiNew ;
082700080207
082800080207         exsr  AGGIOindirizzo   ;
082900080207
083000080207         cpotel=s_tel  ;
083100080207         cpopiv=s_piv  ;
083200080207         cpocdf= s_cdf  ;
083300080219         if S_piv=*blanks and %subst(S_cdf:12:5)=*blanks and s_cdf<>*blanks;
083400080219         cpopiv=s_cdf  ;
083500080219         endif    ;
083600080207
083700080228 1        if     s_ateco =*blanks     ;
083800080228            exsr   CercaSIC   ;
083900080228          endif ;
0840000802074         if     s_ateco <>*blanks     ;
084100080207            EXSR   Tabella_CMA  ;
084200080207          endif  ;
084300080207
084400080207          if cposct=0 ;
084500080207          cposct=00021 ;
0846000802074         endif               ;
084700080207
084800080207         cpodaq=%dec(datasys:*iso) ;
084900080207         cpodtr=%dec(datasys:*iso) ;
085000080207
085100080211         TipoChiamata='F'    ;
085200080207         exsr CALLtisi   ;
085300080207
085400080207         write tncpo000  ;
085500080207
085600080207
085700080207       ENDSR  ;
085800080206       //-------------------------------------------------------------
085900080206       BEGSR  ELABfiliale  ;
086000080206       // Aggiorno tutto l'indirizzo se il potenziale non ha clienti
086100080206       //  o visite aperte sopra
086200080206
0863000802060      if m_duns<>*blanks          ;
086400080206
086500080214       chain(N)  kcpo    CNACO16L  ;
0866000802061      if not %found(cnaco16l)   ;
086700080219         chain(N)    kcpo    TFACO16L  ;
0868000802062        if not %found(tfaco16l)   ;
086900080206
087000080206           chain    M_duns  WFDEB1FIL  ;
0871000802063          if %found(wfdeb1fil)  ;
087200080206              dsfil= debf_rec     ;
087300080206              DsIndirizzo=F_indirizzo ;
087400080206              exsr    AGGIOindirizzo   ;
087500080206
0876000802063           endif               ;
0877000802062        endif               ;
0878000802061      endif               ;
0879000802060      endif               ;
088000080206
088100080206       // con il duns di sede imposto la partita iva e codice fiscale
088200080206              chain  M_duns_sed   wfdeb1sed ;
0883000802191             if %found(wfdeb1sed)  ;
088400080206                DsSed= debs_rec     ;
088500080219 2              if    cpopiv=*blanks  ;
088600080206                cpopiv=s_piv  ;
088700080219         if S_piv=*blanks and %subst(S_cdf:12:5)=*blanks and s_cdf<>*blanks;
088800080219         cpopiv=s_cdf  ;
088900080219         endif    ;
089000080213                endif   ;
089100080219 2              if    cpocdf= *blanks  ;
089200080206                cpocdf= s_cdf  ;
089300080213                endif   ;
0894000802191             endif               ;
089500080206
089600080206       ENDSR   ;
089700080206       //-------------------------------------------------------------
089800080206       BEGSR  ELABSede     ;
089900080206       // Aggiorno dati anagrafici se il potenziale non ha clienti
090000080206       //  o visite aperte sopra
090100080219
090200080219       cpoduns = M_duns   ;
090300080219       cpoFSF = 'F'      ;
090400080220       cpodtr=%dec(datasys:*iso) ;
090500080206
090600080206       chain    M_duns  WFDEB1SED  ;
0907000802061      if %found(wfdeb1sed) ;
090800080219
090900080206       DsSed= debs_rec     ;
091000080219       DsIndirizzo=S_indirizzo ;
091100080220
091200080220       exsr   AggioDatiSede     ;
091300080220
0914000802061      endif               ;
091500080206
091600080206       ENDSR   ;
091700080220       //-------------------------------------------------------------
091800080220       BEGSR  AggioDATISede         ;
091900080220
092000080220       // non la devo aver già aggiornata e deve avere la stessa prv
092100080222       setll   cpoduns   tncpo35l   ;
092200080222       reade   cpoduns   tncpo35l   ;
092300080222       dow    not %eof(tncpo35l) and xcpofsf<>'S'      ;
092400080222       reade   cpoduns   tncpo35l   ;
092500080222       enddo   ;
092600080222
0927000802221      if (%eof(tncpo35l) or  xcpofsf<>'S') and
092800080222          I_prv=cpoprv;
092900080222
093000080222       chain(N)  kcpo  CNACO16L  ;
093100080222       chain(N)   kcpo TFACO16L  ;
093200080222
0933000802222      if (not %found(cnaco16l) and not %found(tfaco16l)) or
093400080222              %subst(cporst:1:1)='A'              ;
093500080220       EXSR    AGGIOdatinew   ;
093600080220       cpoFSF = 'S'           ;
0937000802222      endif                  ;
0938000802221      endif                  ;
093900080220
094000080220       // Tabella CMA
0941000802221      if   s_ateco <>*blanks     ;
094200080220            savsct=cposct     ;
094300080220            EXSR   Tabella_CMA  ;
0944000802222           if  savsct<>cposct  ;
094500080220            setll    KCPO  cnaco16l  ;
094600080220            reade    KCPO  cnaco16l  ;
094700080220
0948000802223             dow not %eof(CNACO16L)     ;
0949000802224             if acoitc<>cposct   ;
095000080220              acoitc=cposct       ;
095100080220              clear acoftr   ;
095200080220              clear acodtr   ;
095300080220              update cnaco016 ;
095400080220              else              ;
095500080220              unlock cnaco16l  ;
0956000802224             endif ;
095700080220            reade    KCPO  cnaco16l  ;
0958000802223             enddo ;
095900080220
096000080220            setll    KCPO  tfaco16l  ;
096100080220            reade    KCPO  tfaco16l  ;
096200080220
0963000802223             dow not %eof(tfACO16L)     ;
0964000802224             if acoitc<>cposct   ;
096500080220              acoitc=cposct       ;
096600080220              clear acoftr   ;
096700080220              clear acodtr   ;
096800080220              update tfaco016 ;
096900080220              else              ;
097000080220              unlock tfaco16l  ;
0971000802224             endif ;
097200080220            reade    KCPO  tfaco16l  ;
0973000802223             enddo ;
0974000802222           endif ;
0975000802221      endif               ;
097600080220
0977000802221      if cpopiv= *blanks ;
097800080220       cpopiv=s_piv  ;
097900080220         if S_piv=*blanks and %subst(S_cdf:12:5)=*blanks and s_cdf<>*blanks;
098000080220         cpopiv=s_cdf  ;
098100080220         endif    ;
0982000802221      endif     ;
098300080220
0984000802221      if cpocdf= *blanks ;
098500080220       cpocdf= s_cdf  ;
0986000802221      endif          ;
098700080220
098800080220       chain(N)  kcpo    CNACO16L  ;
0989000802221      if not %found(cnaco16l)   ;
099000080220         chain(N)   kcpo TFACO16L  ;
0991000802222        if not %found(tfaco16l)   ;
099200080220
099300080220              exsr    AGGIOindirizzo   ;
099400080220       // Il telefono lo aggiorno solo se  pieno altrimenti lascio in vecchi
0995000802223             if s_tel<>*blanks   ;
099600080220              cpotel=s_tel ;
0997000802223             endif   ;
0998000802222        endif               ;
0999000802221      endif               ;
100000080220       ENDSR               ;
100100080220
100200080206       //-------------------------------------------------------------
100300080206        BEGSR   AggioIndirizzo  ;
100400080213        // se manca il CAP  tengo quello presente se stessa provincia
100500080213              if i_cap=*blanks or i_cap='00000    '  ;
100600080213               savcap=cpocap   ;
100700080213               savprv=cpoprv   ;
100800080213              endif  ;
100900080213
101000080206              cporag = i_rag     ;
101100080213              cpocap = i_cap          ;
101200080213              cpoprv = i_prv     ;
101300080214              cpovia = i_via     ;
101400080213
1015000802131             if  i_fraz= *blanks or i_fraz=i_cit  ;
101600080207                cpocit = i_cit     ;
101700080212x2            else   ;
101800080212              //Verifico se frazione corretta --> al posto della local
101900080212       clear   tisi95ds  ;
102000080212       clear   fnlv13ds  ;
102100080212       i95tcn='7'        ;
102200080212       i13la3='S'     ;
102300080212       i95loc=i_fraz  ;
102400080212       i95prv=i_prv   ;
102500080212       i95dat=%dec(datasys:*iso) ;
102600080212       i13af0='S'     ;
102700080212       i13cnv=' '     ;
102800080212       i13af1=' '     ;
102900080212 2       if (i_regione='SAN MARINO'  and i_prv='  ') or
103000080212            (i_cit='REPUBBLICA DI SAN MARINO' AND i_prv='  ');
103100080212            i_prv='RN'  ;
103200080212            I95PRV='RN'  ;
103300080212 2       endif  ;
103400080212
103500080212       callp   FNLV13R  (kpjba:fnlv13ds:tisi95ds) ;
103600080212 2     if   o95err=' '        ;
103700080213                cpocit = o95loc    ;
103800080213                cpocap = o95cap    ;
103900080221                cpoprv = o95prv    ;
104000080212 x2    else   ;
104100080206
104200080207                clear w036a;
104300080206                  w036a=%trim(I_via) +'-' +%trim(I_fraz) ;
104400080212 3                if %subst(w036a:36:1)=' ' ;
104500080206                    cpovia = w036a     ;
104600080206                    cpocit = i_cit     ;
104700080212x3                else   ;
104800080212                    cpocit = i_cit     ;
1049000802123                 endif               ;
1050000802122               endif               ;
1051000802121             endif               ;
105200080206
105300080213
105400080213 1           if cpocap=*blanks or cpocap='00000    '   ;
105500080213              clear   tisi95ds  ;
105600080213              clear   fnlv13ds  ;
105700080213              i95tcn='7'        ;
105800080213              i13la3='S'     ;
105900080213              i95loc=cpocit  ;
106000080213              i95prv=cpoprv  ;
106100080213              i95dat=%dec(datasys:*iso) ;
106200080213              i13af0='S'     ;
106300080213              i13cnv=' '     ;
106400080213              i13af1=' '     ;
106500080213 2            if (i_regione='SAN MARINO'  and i_prv='  ') or
106600080213               (i_cit='REPUBBLICA DI SAN MARINO' AND i_prv='  ');
106700080213               i_prv='RN'  ;
106800080213               I95PRV='RN'  ;
106900080213 2            endif  ;
107000080213
107100080213              callp   FNLV13R  (kpjba:fnlv13ds:tisi95ds) ;
107200080213 2            if   o95err=' '        ;
107300080213               cpocap = o95cap         ;
107400080221               cpoprv = o95prv         ;
107500080213              else    ;
107600080213                if savprv=cpoprv   ;
107700080213                 cpocap=savcap   ;
107800080213                endif        ;
107900080213 2            endif   ;
108000080213 1            endif   ;
108100080206       ENDSR           ;
108200080207       //-------------------------------------------------------------
108300080207       BEGSR   AggioDatiNew  ;
1084000802072       if S_annfat='0   ' or S_annfat='    ' ;
108500080207       cpoafaz  =0 ;
108600080207        else  ;
108700080207       cpoafaz  =%int(s_annfat)   ;
1088000802072       endif ;
108900080207       wleft=%check(Numeri:S_totfat);
1090000802072       select   ;
109100080207        when wleft=1               ;
109200080207       clear cpoftaz     ;
109300080207        when wleft=0 ;
109400080207       cpoftaz=%int(S_totfat) ;
109500080207        other ;
109600080207       cpoftaz=%int(%subst(S_totfat:1:(wleft-1))) ;
109700080207 2      endsl  ;
109800080207
109900080207 2      if cpoftaz  >0 and cpoffaz<>'O'  ;
110000080207 3       select  ;
110100080207         when   s_dicfat<>' '   ;
110200080207         cpoffaz  ='D'          ;
110300080207         when   s_stifat<>' '   ;
110400080207         cpoffaz  ='S'          ;
110500080207         other                  ;
110600080207         cpoffaz  ='B'          ;
110700080207 3       endsl                  ;
110800080207 2      endif   ;
110900080207
111000080207       wleft=%check(Numeri:S_export);
1111000802072       select   ;
111200080207        when wleft=1               ;
111300080207       clear cpopexp     ;
111400080207        when wleft=0 ;
111500080207       cpopexp  =%int(S_export) ;
111600080207        other   ;
111700080207       cpopexp  =%int(%subst(S_export:1:(wleft-1))) ;
1118000802072      endsl  ;
111900080207
112000080207       ENDSR;
112100071122       //-------------------------------------------------------------
112200071122       BEGSR  CALLTisi      ;
112300071122
112400071122       clear   tisi95ds  ;
112500071122       clear   fnlv13ds  ;
112600071122       i95tcn='7'        ;
112700071122       i13la3='S'     ;
112800080207       i95cap=i_cap   ;
112900080215       if i_fraz<>*blanks  ;
113000080215       i95loc=i_fraz  ;
113100080215       clear    i95cap;
113200080215       else  ;
113300080215       i95loc=i_cit   ;
113400080215       endif    ;
113500080207       i95prv=i_prv   ;
113600071122       i95dat=%dec(datasys:*iso) ;
113700071122       i13af0='S'     ;
113800071129       i13cnv=' '     ;
113900071129       i13af1=' '     ;
114000080211 1       if (i_regione='SAN MARINO'  and i_prv='  ') or
114100080207            (i_cit='REPUBBLICA DI SAN MARINO' AND i_prv='  ');
114200080207            i_prv='RN'  ;
114300071211            I95PRV='RN'  ;
114400080211 1       endif  ;
114500071211
114600071122       callp   FNLV13R  (kpjba:fnlv13ds:tisi95ds) ;
114700071122
114800080211 1     if Tipochiamata='L' ;
114900080215         if  I_fraz=*blanks ;
115000080215 2         if o95err=' '       ;
115100080215           DeBloc=o95loc       ;
115200080219           DeBlprv=o95prv       ;
115300080215           else                ;
115400080215           DeBloc=i_cit        ;
115500080219           DeBlprv=I_prv        ;
115600080215 2         endif               ;
115700080215         else   ;
115800080215 2         if o95err=' '       ;
115900080215           DeBfraz=o95loc       ;
116000080219           DeBfprv=o95prv       ;
116100080215           else                ;
116200080215           DeBfraz=i_fraz       ;
116300080219           DeBfprv=I_prv        ;
116400080215 2         endif               ;
116500080215         endif  ;
116600080211 x1    else                ;
116700080207       cpoflt=o95lna       ;
116800080211 1     endif               ;
116900080211
117000080211 1     if     (Tipochiamata='L'  and I_fraz<>*blanks)   or
117100080211              (Tipochiamata='F' and cpoflt=0 and i_fraz<>*blanks) ;
117200080207       clear   i95loc  ;
117300080215       i95loc   =i_cit ;
117400080215       i95cap   =i_cap ;
117500080207
117600080207       callp   FNLV13R  (kpjba:fnlv13ds:tisi95ds) ;
117700080207
117800080211
117900080211 2     if Tipochiamata='L' ;
118000080211 3       if o95err=' '       ;
118100080215         DeBloc=o95loc       ;
118200080219         DeBlprv=o95prv       ;
118300080211         else                ;
118400080215         DeBloc=i_cit        ;
118500080219         DeBlprv=I_prv        ;
118600080211 3       endif               ;
118700080211 x2    else                ;
118800080211         cpoflt=o95lna       ;
118900080211 2     endif   ;
119000080211 1     endif   ;
119100071122
119200071122       ENDSR                ;
119300071122
119400080211       //-------------------------------------------------------------
119500080226       BEGSR  CALLTisiCPO   ;
119600080211
119700080211       clear   windloc  ;
119800080219       clear   windprv  ;
119900080211       clear   tisi95ds  ;
120000080211       clear   fnlv13ds  ;
120100080211       i95tcn='7'        ;
120200080211       i13la3='S'     ;
120300080219       i95cap=CPOCAP  ;
120400080219       i95loc=cpocit  ;
120500080219       i95prv=cpoprv  ;
120600080211       i95dat=%dec(datasys:*iso) ;
120700080211       i13af0='S'     ;
120800080211       i13cnv=' '     ;
120900080211       i13af1=' '     ;
121000080211
121100080211       callp   FNLV13R  (kpjba:fnlv13ds:tisi95ds) ;
121200080211
121300080211 1       if o95err=' '       ;
121400080211         wINDloc=o95loc       ;
121500080219         wINDprv=o95prv       ;
121600080211         else                ;
121700080219         wINDloc=cpocit       ;
121800080219         wINDprv=cpoprv       ;
121900080211 1       endif               ;
122000080211
122100080211       ENDSR                ;
122200080226       //-------------------------------------------------------------
122300080226       BEGSR  CALLTisiIND   ;
122400080226
122500080226       clear   tisi95ds  ;
122600080226       clear   fnlv13ds  ;
122700080226       i95tcn='7'        ;
122800080226       i13la3='S'     ;
122900080226       i95cap=INDCAE  ;
123000080226       i95loc=INDcit  ;
123100080226       i95prv=INDprv  ;
123200080226       i95dat=%dec(datasys:*iso) ;
123300080226       i13af0='S'     ;
123400080226       i13cnv=' '     ;
123500080226       i13af1=' '     ;
123600080226
123700080226       callp   FNLV13R  (kpjba:fnlv13ds:tisi95ds) ;
123800080226
123900080226       ENDSR                ;
124000071123       //-------------------------------------------------------------
124100071123       BEGSR  Tabella_CMA   ;
124200071123        tbecod='CMA'     ;
124300080206        tbeke1=s_ateco ;
124400071123        CHAIN   (tbecod:tbeke1)   TNTBE01L  ;
124500071128          if   %found(TNTBE01l)  ;
124600071123        DCMA = tbeuni  ;
124700080207        cposct   = %int(§CMAmerc) ;
124800080206          endif  ;
124900071123       ENDSR                ;
125000080228       //-------------------------------------------------------------
125100080228       BEGSR  CercaSIC      ;
125200080228
125300080228       setll  *start   nscateco ;
125400080228       read   nscateco   ;
125500080228       dow    not %eof(nscateco)   and s_ateco=*blanks ;
125600080228         if S_sic = campo1;
125700080228         s_ateco =campo2;
125800080228         endif   ;
125900080228
126000080228       read   nscateco   ;
126100080228       enddo  ;
126200080228
126300080228       ENDSR                ;
126400071130
126500071116      /end-free
126600080226**
126700080226FOFCOTSSORNUCAOGCIVSSVCNIMPUPS
