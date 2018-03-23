000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300051012     F* FILE NUOVO
000400161113     fFIART01L  uf   e           k Disk    usropn
000500161116     ffnbrv07l  if   e           k Disk
000600051012     F
000700051012     D
000800161113     d fnlru39ds     e ds
000900161113     d fiartds       e ds                  extname(fiart00f)
001000161113     d s_fiartds     e ds                  extname(fiart00F) prefix(s_)
001100060215     d
001200051012     C
001300161116     c     kart          klist
001400161116     c                   kfld                    artfls
001500161116     c                   kfld                    artlna
001600161116     c                   kfld                    artnrs
001700161116     c                   kfld                    artnsc
001800060217      *
001900090225     c                   open      fiart01l
002000051019     c
002100980130     C***
002200161113     C     *LOVAL        SETLL     Fiart01L
002300161113     C                   READ      Fiart01L
002400051012     C
002500161113    1C                   DOW       NOT %EOF(Fiart01L)
002600161116    2c                   if        artdcm>20161101
002700161116     c     kart          chain     fnbrv07l
002800161116    3c                   if        %found(fnbrv07l) and brvnvr=' '
002900161113     c                   clear                   fnlru39ds
003000161113     c                   eval      i38aas=artaas
003100161113     c                   eval      i38lnp=artlnp
003200161113     c                   eval      i38nrs=artnrs
003300161113     c                   eval      i38nsp=artnsp
003400161113     c                   eval      i38nsc=artnsc
003500161113     c                   call      'FNLRU39R'
003600161113     c                   parm                    fnlru39ds
003700161113     c                   parm                    s_fiartds
003800161113     c* In base ai campi di fiart già aggiornati dai pgm e che
003900161113     c* quindi non devo reperire valorizzo solo i campi di fiart che mi interessano
004000161113     c* ora li valorizzo tutti
004100161113     c                   eval      fiartds=s_fiartds
004200161113     c                   update    fiart000
004300161104     c*
004400161116    3c                   endif
004500161116    2c                   endif
004600051012     C
004700161113     C                   READ      Fiart01L
004800161116    1c                   enddo
004900051012     C
005000161113
005100161113     c                   movel     'C'           i38tla
005200161113     c                   call      'FNLRU39R'
005300161113     c                   parm                    fnlru39ds
005400161113     c                   parm                    s_fiartds
005500120618     c
005600020502     C                   SETON                                        LR
