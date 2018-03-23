000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000201150910     Ffiar531c  uF a E           k DISK    extfile(wta531) usropn
000408111012
000700090715     d
000800150831     d dAr5gen       e ds                  Inz
000801150831     d dAr5doc       e ds                  Inz
000903150910     d Wta531          s             21    inz('GAITRAGRPS/FIAR531C')
000904150831     d SQL_ar5uni      s                   like(ar5uni)
000905150831     d SQL_ar5aas      s                   like(ar5aas)
000906150831     d SQL_ar5lnp      s                   like(ar5lnp)
000907150831     d SQL_ar5nrs      s                   like(ar5nrs)
000908150831     d SQL_ar5nsp      s                   like(ar5nsp)
000909150910     d wrkgetlista     s           4096    varying
000910150910     d Dataiso         s               d   datfmt(*iso)
000911150910     d TimeIso         s               t   timfmt(*iso)
001500100317
001600011003     C*------------------------------------------------------------------------*
001700011003     C* MAIN LINE
001800011003     C*------------------------------------------------------------------------*
002600100317     c
003100100317     c
003101150831      /free
003102150831         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
003103150831
003109150831      /end-free
004408111012     c*
004414150831     c                   Open(e)   fiar531c
004415150831     c                   if        not %open(fiar531c)
004416150831     c                   eval      %subst(Wta531:7:4)='GRU '
004417150831     c                   Open      fiar531c
004418111012     c                   endif
004500011003     C*
004501150831      /free
004502150831        wrkgetlista ='select ar5aas, ar5lnp, ar5nrs, ar5nsp, ar5uni +
004503150910                     from ' + %subst(WTA531:1:10)
004504150910                     + '/fiar500f WHERE ar5trd=''GEN''  '      ;
004505150831       EXEC sql  prepare s1 from : wrkgetlista ;
004506150831       EXEC sql  declare a1 cursor for s1      ;
004507150831       EXEC sql  open    a1                    ;
004508150831
004509150831       EXEC sql fetch next from a1 into :sql_ar5aas, :sql_ar5lnp, :sql_ar5nrs,
004510150831                                        :sql_ar5nsp, :sql_ar5uni           ;
004511150831
004512150831
004513150831       dow sqlcod=0    ;
004514150831
004515150831       dar5gen=sql_ar5uni;
004516150831       if 你R5DRDTO<>*blanks or
004517150831          你R5FSTDO<>*blanks or
004518150831          你R5MTVDTO<>*blanks or
004519150831          你R5DIMTVO<>*blanks   ;
004520150831          chain (sql_ar5aas:sql_ar5lnp:sql_ar5nrs:sql_ar5nsp:'DOC') fiar531c;
004521150831 1        if %found(fiar531c)  ;
004522150831             dar5doc=ar5uni           ;
004525150831          else;
004526150831             exsr ImpostaAR5          ;
004527150831          endif;
004528150831          你R5DRDT =你R5DRDTo  ;
004529150831          你R5FSTD =你R5FSTDo  ;
004530150831          你R5MTVDT=你R5MTVDTo ;
004531150831          你R5DIMTV=你R5DIMTVo ;
004532150831
004533150831          ar5uni=dar5doc  ;
004534150831          exsr UpdateAR5  ;
004535150831       endif;
004536150831
004537150831       EXEC sql fetch next from a1 into :sql_ar5aas, :sql_ar5lnp, :sql_ar5nrs,
004538150831                                        :sql_ar5nsp, :sql_ar5uni           ;
004539150831       enddo           ;
004540150831
004541150831       EXEC sql  CLOSE   a1                    ;
004542150910       *inlr=*on;
004543150831       //--------------------------------------------------------------
004544150831       //?Imposta campi FIar5 per la write
004545150831       //--------------------------------------------------------------
004546150831       BEGSR ImpostaAR5         ;
004547150831
004548150831       clear fiar5000           ;
004549150910       ar5aas=sql_ar5aas          ;
004550150910       ar5lnp=sql_ar5lnp          ;
004551150910       ar5nrs=sql_ar5nrs          ;
004552150910       ar5nsp=sql_ar5nsp          ;
004553150831       ar5trd='DOC'             ;
004554150831       dataiso=%date             ;
004555150831       ar5dac=%dec(dataiso)     ;
004556150831       timeiso=%time             ;
004557150831       ar5orc=%dec(timeiso)     ;
004558150831       ar5ft1='R'               ;
004559150831       ar5dt1=%dec(dataiso)     ;
004560150831       ar5ft2='R'               ;
004561150831       ar5dt2=%dec(dataiso)     ;
004562150831       ar5ft3='R'               ;
004563150831       ar5dt3=%dec(dataiso)     ;
004564150831
004565150831       clear dar5doc            ;
004566150831       ENDSR                   ;
004567150831       //--------------------------------------------------------------
004568150831       //?Aggiorno record FIAR5
004569150831       //--------------------------------------------------------------
004570150831       BEGSR  UpdateAR5  ;
004571150831
004572150831 2     if not %found(fiar531c)  ;
004576150831         write  fiar5000           ;
004578150831
004579150831 x2    else   ;
004583150831          update fiar5000           ;
004585150831 2     endif    ;
004586150831
004587150831       ENDSR  ;
