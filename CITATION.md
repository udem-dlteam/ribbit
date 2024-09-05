To cite this software, please cite the following publications:

- **Léonard Oest O’Leary, Mathis Laroche, and Marc Feeley. 2023. A R4RS Compliant REPL in 7 KB. (2023). https://doi.org/10.48550/ARXIV.2310.13589 arXiv:2310.13589 [cs.PL] This paper was presented at the Scheme and Functional Programming Workshop (SFPW’23), part of ICFP’23.**
```bibtex
@article{
  OestOlearyLarocheFeeley2023,
  author       = {L{\'{e}}onard Oest O'Leary and Mathis Laroche and Marc Feeley},
  title        = {A R4RS Compliant REPL in 7 KB},
  year         = {2023},
  location     = {Seattle, WA, USA},
  eprint       = {2310.13589},
  series       = {SFPW'23},
  doi          = {10.48550/ARXIV.2310.13589},
  url          = {https://arxiv.org/abs/2310.13589},
  keywords     = {Virtual Machines, Compiler, Dynamic Languages, Scheme, Compactness},
  primaryclass = {cs.PL},
  note         = {This paper was presented at the Scheme and Functional Programming Workshop (SFPW'23), part of ICFP'23}
}
```

- **Léonard Oest O'Leary and Marc Feeley. 2023. A Compact and Extensible Portable Scheme VM. In Companion Proceedings of the 7th International Conference on the Art, Science, and Engineering of Programming (Programming '23). Association for Computing Machinery, New York, NY, USA, 3–6. https://doi.org/10.1145/3594671.3594672**
```bibtex
@inproceedings{
    OestOlearyFeeley2023,
    author = {O'Leary, L\'{e}onard Oest and Feeley, Marc},
    title = {A Compact and Extensible Portable Scheme VM},
    year = {2023},
    isbn = {9798400707551},
    publisher = {Association for Computing Machinery},
    address = {New York, NY, USA},
    url = {https://doi.org/10.1145/3594671.3594672},
    doi = {10.1145/3594671.3594672},
    abstract = {Virtual Machines (VM) tend to evolve over their life cycle with features being added regularly and a growing footprint. In a VM designed for resource constrained environments this trend deteriorates the VM’s primary quality. We present how extensibility is implemented in the Ribbit Scheme VM that is both compact and portable to multiple languages. Our approach adds annotations to the VM’s source code allowing the compiler to generate the source code of a specialized VM extended with user-defined primitives and with needless ones removed. This gives the best of both worlds: an extensible VM packed with all and only the features needed by the source code, while maintaining a small code footprint.},
    booktitle = {Companion Proceedings of the 7th International Conference on the Art, Science, and Engineering of Programming},
    pages = {3–6},
    numpages = {4},
    keywords = {Compactness, Compiler, Dynamic Languages, Scheme, Virtual Machines},
    location = {Tokyo, Japan},
    series = {Programming '23}
}
```

- **Samuel Yvon and Marc Feeley. 2021. A small scheme VM, compiler, and REPL in 4k. In Proceedings of the 13th ACM SIGPLAN International Workshop on Virtual Machines and Intermediate Languages (VMIL 2021). Association for Computing Machinery, New York, NY, USA, 14–24. https://doi.org/10.1145/3486606.3486783**
```bibtex
@inproceedings{
  YvonFeeley2021,
  author = {Yvon, Samuel and Feeley, Marc},
  title = {A small scheme VM, compiler, and REPL in 4k},
  year = {2021},
  isbn = {9781450391092},
  publisher = {Association for Computing Machinery},
  address = {New York, NY, USA},
  url = {https://doi.org/10.1145/3486606.3486783},
  doi = {10.1145/3486606.3486783},
  abstract = {Compact language implementations are increasingly popular for use in resource constrained environments. For embedded applications such as robotics and home automation, it is useful to support a Read-Eval-Print-Loop (REPL) so that a basic level of interactive development is possible directly on the device. Due to its minimalistic design, the Scheme language is particularly well suited for such applications and several implementations are available with different tradeoffs. In this paper we explain the design and implementation of Ribbit, a compact Scheme system that supports a REPL, is extensible and has a 4 KB executable code footprint.},
  booktitle = {Proceedings of the 13th ACM SIGPLAN International Workshop on Virtual Machines and Intermediate Languages},
  pages = {14–24},
  numpages = {11},
  keywords = {Virtual Machines, Small Footprint, Scheme, Read-Eval-Print-Loop, Dynamic Languages, Compiler},
  location = {Chicago, IL, USA},
  series = {VMIL 2021}
}
```
