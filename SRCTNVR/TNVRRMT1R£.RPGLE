000100031205      *------------------------------------------------------------------------*
000200031205      *------------------------------------------------------------------------*
000300071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000400031205      *------------------------------------------------------------------------*
000500120928      *------------------------------------------------------------------------*
000600121018     ffnblp32l  if   e           k Disk
000601121018     fgrf121017 if   e           k Disk    rename(grf121017:grf000)
000602121018     ffiar501l  uf   e           k Disk
000603121018     ffiar531c  uf   e           k Disk    extfile(wlib)
000604121018     f                                     rename(fiar5000:fiar5sed) usropn
000605121018     f
000700121001     fqsysprt   o    f  132        printer
000701121018     D Wlib            S             21
000702121018     D Wlibsedep       S             21    inz('GAITRAGRPS/FIAR531C')
000703121018     D Wlibsedeb       S             21    inz('GAITRAGRU /FIAR531C')
000704121018     d Dar5Gen       e Ds
000705121018     d kpjba         e Ds
000800121018     D kaas            s              4  0 inz(2012)
000801121018     D ktrc            s                   like(ar5trd) inz('GEN')
000802121018     c     *entry        plist
000803121018     c                   parm                    kpjba
000804121018
000805121018     C     kblp          klist
000806121018     C                   kfld                    vabrma
000807121018     C                   kfld                    kaas
000808121018     C     kar5          klist
000809121018     C                   kfld                    blpaas
000810121018     C                   kfld                    blplnp
000811121018     C                   kfld                    blpnrs
000812121018     C                   kfld                    blpnsp
000813121018     C                   kfld                    ktrc
000814121018      /free
000815121018       if %subst(knsif:7:1) = 'P';
000816121018         Wlib = Wlibsedep;
000817121018         Else ;
000818121018         wlib = Wlibsedeb;
000819121018       endif;
000820121018       Open   Fiar531c;
000821121018      /end-free
001200121018     c* leggo file del cliente contenente i numeri di telefono da memorizzare
001400121018     C                   read      grf121017
001500121018     c                   dow       not %eof(grf121017)
001501121018     c                   clear                   wtrov             1
001502121018     c* con rma cerco la bolla da aggiornare
001503121018     c     kblp          setll     fnblp32l
001504121018     c     kblp          reade     fnblp32l
001505121018     c                   dow       not %eof (fnblp32l)
001506121018     c* se stesso cliente aggiorno fiar5 record GEN
001507121018     c                   if        vabccm=blpksc
001508121018     c                   exsr      aggar5gen
001509121018     c                   eval      wtrov='S'
001510121018     c                   leave
001511121018     c                   endif
001512121018     c     kblp          reade     fnblp32l
001513121018     c                   enddo
001514121018     c* se non trovata bolla stampo
001515121018     c                   if        wtrov=*blanks
001516121018     c                   except    riga
001517121018     c                   endif
003200121018     C                   read      grf121017
003300121001     c                   enddo
003304121001     c
005600121001     c                   seton                                        lr
005601121018     c***************************************************************************
005602121018     c* aggiornamento fiar5 record 'GEN'
005701121018     c***************************************************************************
005702121018     C     aggar5gen     begsr
005703121018     c* aggiorno prima fiar5 di filiale e poi ar5 di sede
005705121018     c                   exsr      ar5genfil
005707121018     c* fiar5 di sede
005708121018     c                   exsr      ar5gensede
005709121018     c                   endsr
005710121018     c***************************************************************************
005711121018     c* aggiornamento fiar5 record 'GEN' file di filiale
005712121018     c***************************************************************************
005713121018     C     ar5genfil     begsr
005714121018     c     kar5          chain     fiar501l
005715121018     c                   if        not %found(fiar501l)
005716121018     c                   except    riga1
005717121018     c                   else
005718121018     c                   eval      dar5gen=ar5uni
005719121018     c                   if        §ar5ref<>*blanks
005720121018     c                   except    riga2
005721121018     c                   unlock    fiar501l
005722121018     c                   else
005723121018     c                   eval      §ar5ref=telefoni
005724121018     c                   eval      ar5uni=dar5gen
005725121018     c                   update    fiar5000
005726121018     c                   endif
005727121018     c                   endif
005728121018     C                   endsr
005729121018     c***************************************************************************
005730121018     c* aggiornamento fiar5 record 'GEN' file di sede
005731121018     c***************************************************************************
005732121018     C     ar5gensede    begsr
005739121018     c     kar5          chain     fiar531c
005740121018     c                   if        not %found(fiar531c)
005741121018     c                   except    riga11
005742121018     c                   else
005743121018     c                   eval      dar5gen=ar5uni
005744121018     c                   if        §ar5ref<>*blanks
005745121018     c                   except    riga12
005746121018     c                   unlock    fiar531c
005747121018     c                   else
005748121018     c                   eval      §ar5ref=telefoni
005749121018     c                   eval      ar5uni=dar5gen
005750121018     c                   update    fiar5sed
005751121018     c                   endif
005752121018     c                   endif
005753121018     C                   endsr
005800121001     OQSYSPRT   E            riga        1
005900121001     O                                              'sped.:'
006000121018     O                       vabrma           +   1
006100121018     O                       vabccm           +   2
006101121018     O          E            riga1       1
006102121018     O                                              'sped.:'
006103121018     O                       vabrma           +   1
006104121018     O                       vabccm           +   2
006200121018     o                                              'Non trovato fiar5'
006300121018     O          E            riga2       1
006400121018     O                                              'sped.:'
006500121018     O                       vabrma           +   1
006600121018     O                       vabccm           +   2
006700121018     o                                              'Referente pieno'
006800121018     O          E            riga11      1
006900121018     O                                              'sped.:'
007000121018     O                       vabrma           +   1
007100121018     O                       vabccm           +   2
007200121018     o                                              'Non trovato fiar5 sede'
007300121018     O          E            riga12      1
007400121018     O                                              'sped.:'
007500121018     O                       vabrma           +   1
007600121018     O                       vabccm           +   2
007700121018     o                                              'Referente pieno - sede'
