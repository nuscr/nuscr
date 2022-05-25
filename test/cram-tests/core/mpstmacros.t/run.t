Printing global types in latex using mpstmacros.

Streaming protocol looks like this:
μ(t)(
DP→K:d(bool) .
KP→K:k(bool) .
K→C:c(bool) .
DP→K:d(bool) .
KP→K:k(bool) .
K→C:c(bool) . t
)
  $ nuscr --show-global-type-tex Streaming Streaming.nuscr
  \gtRec{t}{\gtCommSingle{\RoleFmt{DP}}{\RoleFmt{K}}{\LabelFmt{d}}{\PayloadFmt{bool}}{%
  \gtCommSingle{\RoleFmt{KP}}{\RoleFmt{K}}{\LabelFmt{k}}{\PayloadFmt{bool}}{%
  \gtCommSingle{\RoleFmt{K}}{\RoleFmt{C}}{\LabelFmt{c}}{\PayloadFmt{bool}}{%
  \gtCommSingle{\RoleFmt{DP}}{\RoleFmt{K}}{\LabelFmt{d}}{\PayloadFmt{bool}}{%
  \gtCommSingle{\RoleFmt{KP}}{\RoleFmt{K}}{\LabelFmt{k}}{\PayloadFmt{bool}}{%
  \gtCommSingle{\RoleFmt{K}}{\RoleFmt{C}}{\LabelFmt{c}}{\PayloadFmt{bool}}{%
  \gtRecVar{t}
  }
  }
  }
  }
  }
  }}

Two buyer protocol looks like this:
B1→S:s(string) .
S→B1:b1(int) .
S→B2:b2(int) .
B1→B2:bi2(int) .
B2→S:{
ok . B2→S:s(string) . S→B2:b2(string) . end,
quit . end
}
  $ nuscr --show-global-type-tex TwoBuyer TwoBuyer.nuscr
  \gtCommSingle{\RoleFmt{B1}}{\RoleFmt{S}}{\LabelFmt{s}}{\PayloadFmt{string}}{%
  \gtCommSingle{\RoleFmt{S}}{\RoleFmt{B1}}{\LabelFmt{b1}}{\PayloadFmt{int}}{%
  \gtCommSingle{\RoleFmt{S}}{\RoleFmt{B2}}{\LabelFmt{b2}}{\PayloadFmt{int}}{%
  \gtCommSingle{\RoleFmt{B1}}{\RoleFmt{B2}}{\LabelFmt{bi2}}{\PayloadFmt{int}}{%
  \gtCommRaw{\RoleFmt{B2}}{\RoleFmt{S}}{%
  \begin{array}{@{}l@{}}
  \commChoice{\LabelFmt{ok}}{}{\gtCommSingle{\RoleFmt{B2}}{\RoleFmt{S}}{\LabelFmt{s}}{\PayloadFmt{string}}{%
  \gtCommSingle{\RoleFmt{S}}{\RoleFmt{B2}}{\LabelFmt{b2}}{\PayloadFmt{string}}{%
  \gtEnd
  }
  }}\\
  \commChoice{\LabelFmt{quit}}{}{\gtEnd}
  \end{array}
  }
  }
  }
  }
  }

Projection of TwoBuyer
  $ nuscr --project-tex B1@TwoBuyer TwoBuyer.nuscr
  \ltIntCSingle{\RoleFmt{S}}{\LabelFmt{s}}{\PayloadFmt{string}}{%
  \ltExtCSingle{\RoleFmt{S}}{\LabelFmt{b1}}{\PayloadFmt{int}}{%
  \ltIntCSingle{\RoleFmt{B2}}{\LabelFmt{bi2}}{\PayloadFmt{int}}{%
  \ltEnd
  }
  }
  }
  $ nuscr --project-tex B2@TwoBuyer TwoBuyer.nuscr
  \ltExtCSingle{\RoleFmt{S}}{\LabelFmt{b2}}{\PayloadFmt{int}}{%
  \ltExtCSingle{\RoleFmt{B1}}{\LabelFmt{bi2}}{\PayloadFmt{int}}{%
  \ltIntCRaw{\RoleFmt{S}}{%
  \begin{array}{@{}l@{}}
  \commChoice{\LabelFmt{quit}}{}{\ltEnd}\\
  \commChoice{\LabelFmt{ok}}{}{\ltIntCSingle{\RoleFmt{S}}{\LabelFmt{s}}{\PayloadFmt{string}}{%
  \ltExtCSingle{\RoleFmt{S}}{\LabelFmt{b2}}{\PayloadFmt{string}}{%
  \ltEnd
  }
  }}
  \end{array}
  }
  }
  }
  $ nuscr --project-tex S@TwoBuyer TwoBuyer.nuscr
  \ltExtCSingle{\RoleFmt{B1}}{\LabelFmt{s}}{\PayloadFmt{string}}{%
  \ltIntCSingle{\RoleFmt{B1}}{\LabelFmt{b1}}{\PayloadFmt{int}}{%
  \ltIntCSingle{\RoleFmt{B2}}{\LabelFmt{b2}}{\PayloadFmt{int}}{%
  \ltExtCRaw{\RoleFmt{B2}}{%
  \begin{array}{@{}l@{}}
  \commChoice{\LabelFmt{quit}}{}{\ltEnd}\\
  \commChoice{\LabelFmt{ok}}{}{\ltExtCSingle{\RoleFmt{B2}}{\LabelFmt{s}}{\PayloadFmt{string}}{%
  \ltIntCSingle{\RoleFmt{B2}}{\LabelFmt{b2}}{\PayloadFmt{string}}{%
  \ltEnd
  }
  }}
  \end{array}
  }
  }
  }
  }
