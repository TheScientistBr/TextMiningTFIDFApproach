# TextMining
The newspaper publications on the Internet increases every day. There are many news agencies, newspapers and magazines with digital publications on the big network. Published documents made available to users who, in turn, use search engines to find them. To deliver the closest searched documents, these documents must be previously indexed and classified. With the huge volume of documents published every day, many researches have been carried out in order to find a way of dealing with the automatic document classification.

The goal of this article is to discuss an automatic classification experience of some journalistic documents published on the Internet. The experiment uses the Vector Model representation. In doing so, we used a real journalistic database, some algorithms widely used in the literature to test the model we discussed here. The article also describes the performance evaluation metrics of these algorithms and the settings required for the reproduction of the same. The results show the method's efficiency and justifies the researches which aim at facilitating and improving the automatic document classification techniques.

### Dataset

You must try the original dataset on [Kaggle.com](https://www.kaggle.com/TheScientistBR/atribuna)


## The Research

Today hundreds of journalistic documents are published daily on the Internet. Experts strive to develop techniques that can improve performance in the classification and understanding of the texts of the published documents.

We have tried to automatically classify the journalistic classes of A Tribuna. There are 21 classes that are shown graphically below.

### About Class and its DNA 

These are the classes and their Chromosome. This was extracted through the Vector Model with the Terms Frequency and the Inverse Document Frequency metrics (TF-IDF).

![All Classes](images/PlotClassAll.png)

### Comparing Files

Let's compare an AT2 Class file with AT2 Class Chromosome.

![AT2 x 004062006at2.txt ](images/classat2Fileat2.png)
---

[The Scientist](http://www.thescientist.com.br)
