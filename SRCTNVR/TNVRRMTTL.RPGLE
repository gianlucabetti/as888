000100000000     H*------------------------------------------------------------------------*
000200091202     H* Controllo  dirottamenti  TTL
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800091202     FWFdir00f50uf a E             DISK    usropn
001100091009     ftitas38c  if   e           k disk    extfile(wtas38) usropn
001101091202     ftitas30c  if   e           k disk    extfile(wtas30) usropn
001102091202     F                                     RENAME(titas000:titas30)
001103091202     F                                     RENAME(titas010:titas1030)
001104091202     F                                     RENAME(titasp00:titasp30)
001105091202     ffnlbl02l  if   e           k disk
007501090807
007502091202     d w_aasd          s              4  0 inz(2009)
007503091202     d w_mgsd          s              4  0 inz(1101)
007504091202     d w_aamgmax       s              8  0 inz(20091130)
007518090825      * - Parametri per pgm. esecuzione comandi di sistema
007519090825     d Qcmd            s             80    inz
007520090825     d Qlen            s             15  5 inz(%len(Qcmd))
007521091009     d Wtas38          s             21    inz('GAITRAGRU /TITAS38C')
007522091202     d Wtas30          s             21    inz('GAITRAGRU /TITAS30C')
007523090825
007524090825      * - Comando per pulizia Work-File
007525090825     d C_Cmd_ClrWF     c                   const('CLRPFM FILE(*LIBL/+
007526091202     d                                                   WFDIR00F50)')
010000000000     C*------------------------------------------------------------------------*
010100000000     C* MAIN LINE
010200000000     C*------------------------------------------------------------------------*
010300000000     C*
010301090805     c     keytas        setll     titas38c
010302090805     c                   read      titas38c
010303090807     c                   dow       not %eof(titas38c) and
010304091202     c                             (tasaas*10000+tasmgs)<=w_aamgmax
010309091202     c                   if        tascca='1'
010310091202     c                             and (tasksc=0504450 or tasksc=0504487)
010311091202     c                             and tasnrs>0
010312091202     c                   exsr      scrivi
010313090807     c                   endif
010314090805     c                   read      titas38c
010315091202     c                   enddo
010316091202     c*
010317091202     c                   feod      wfdir00f50
010318091202     c     *start        setll     wfdir00f50
010319091202     c                   read      wfdir00f50
010320091202     c                   dow       not %eof(wfdir00f50)
010321091202     c     klbl          chain     fnlbl02l
010322091202     c                   if        %found(fnlbl02l)
010323091202     c     keytasf       chain     titas30c
010324091202     c                   if        %found(titas30c)
010325091202     c                   z-add     tasaas        DIRAASf
010326091202     c                   z-add     taslnp        DIRLNPf
010327091202     c                   z-add     tasnrs        DIRNRSf
010328091202     c                   z-add     tasnsp        DIRNSPf
010329091202     c                   z-add     tasmgs        DIRMGSf
010330091203     c                   z-add     taslna        DIRlnaf
010331091202     c                   movel     tasrsd        DIRRSDf
010332091202     c                   movel     tasind        DIRINDf
010333091202     c                   movel     tascad        DIRCADf
010334091202     c                   movel     taslod        DIRLODf
010335091202     c                   movel     tasprd        DIRPRDf
010336091202     c                   movel     tasnzd        DIRNZDf
010337091202     c                   z-add     tasncl        DIRNCLf
010338091202     c                   z-add     taspkb        DIRPKBf
010339091202     c                   z-add     tasvlb        DIRVLBf
010340091202     c                   update    wfdir000
010341091202     c                   endif
010342091202     c                   endif
010343091202     c                   read      wfdir00f50
010344091202     c                   enddo
020500000000     C*
020600000000     C                   SETON                                        LR
020621090807     C*------------------------------------------------------------------------*
020622091202     C* Scrittura workfile
020623090807     C*------------------------------------------------------------------------*
020624091202     C     scrivi        BEGSR
020625090807     c
020626091202     c                   clear                   wfdir000
020627091202     c                   z-add     tasksc        DIRKSC
020628091202     c                   z-add     tasaas        DIRAAS
020629091202     c                   z-add     taslnp        DIRLNP
020630091202     c                   z-add     tasnrs        DIRNRS
020631091202     c                   z-add     tasnsp        DIRNSP
020632091202     c                   z-add     tasmgs        DIRMGS
020633091203     c                   z-add     taslna        DIRlna
020634091202     c                   movel     tasrsd        DIRRSD
020635091202     c                   movel     tasind        DIRIND
020636091202     c                   movel     tascad        DIRCAD
020637091202     c                   movel     taslod        DIRLOD
020638091202     c                   movel     tasprd        DIRPRD
020639091202     c                   movel     tasnzd        DIRNZD
020640091202     c                   z-add     tasncl        DIRNCL
020641091202     c                   z-add     taspkb        DIRPKB
020642091202     c                   z-add     tasvlb        DIRVLB
020643091202     c                   write     wfdir000
020646091202
020647091202
020800090825     c*
020801090807     c                   endsr
120300000000     C*------------------------------------------------------------------------*
120400000000     C* *INZSR - ROUTINE INIZIALE
120500000000     C*------------------------------------------------------------------------*
120600000000     C     *INZSR        BEGSR
123500000000     C*
123600000000     C*--------------------
123700000000     C* CHIAVI DI LETTURA
123800000000     C*--------------------
123801090807     c     keytas        klist
123802090807     c                   kfld                    w_aasd
123803090807     c                   kfld                    w_mgsd
123804091202     c     keytasf       klist
123805091202     c                   kfld                    LBLAAN
123806091202     c                   kfld                    LBLLPN
123807091202     c                   kfld                    LBLNRN
123808091202     c                   kfld                    LBLNSN
123809091202     c     klbl          klist
123810091202     c                   kfld                    diraas
123811091202     c                   kfld                    dirlnp
123812091202     c                   kfld                    dirnrs
123813091202     c                   kfld                    dirnsp
124701090807     c*
130602090825     c* Pulizia file wfdir prima di scriverlo
130603090825     c                   call      'QCMDEXC'
130604090825     c                   parm      C_Cmd_ClrWF   Qcmd
130605090825     c                   parm                    Qlen
130606090825     c
130607091202     c                   open      WFDIR00F50
130608091029
130615091009     c*
130616091009     c                   open(E)   titas38c
130617091009     c                   if        not %open(titas38c)
130618091009     c                   eval      %subst(wtas38:7:4)='GRPS'
130619091009     c                   open      titas38c
130620091009     c                   endif
130621091202
130622091202     c                   open(E)   titas30c
130623091202     c                   if        not %open(titas30c)
130624091202     c                   eval      %subst(wtas30:7:4)='GRPS'
130625091202     c                   open      titas30c
130626091202     c                   endif
131900000000     C*
132000000000     C                   ENDSR
134500000000     O*------------------------------------------------------------------------*
