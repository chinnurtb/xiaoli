from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()

from datetime import datetime

class Setting(db.Model):
    __tablename__ = 'settings'
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(100))
    value = db.Column(db.Text())
    created_at = db.Column(db.DateTime, default=datetime.now)
    updated_at = db.Column(db.DateTime, default=datetime.now)

    def __init__(self, name, value):
        self.name = name
        self.value = value

class Profile(db.Model):
    __tablename__ = 'profiles'
    id = db.Column(db.Integer, primary_key=True)
    uid = db.Column(db.Integer, db.ForeignKey('users.id'))
    grp = db.Column(db.String(40))
    key = db.Column(db.String(100))
    value = db.Column(db.Text())
    created_at = db.Column(db.DateTime, default=datetime.now)
    updated_at = db.Column(db.DateTime, default=datetime.now)

    def __init__(self, uid, grp, key, value):
        self.uid = uid
        self.grp = grp
        self.key = key
        self.value = value
        created_at = datetime.now()
        updated_at = datetime.now()

    @staticmethod
    def load(uid, grp):
        profile = {}
        profiles = Profile.query.filter_by(uid = uid, grp = grp).all()
        for p in profiles:
            profile[p.key] = p.value
        return profile

    @staticmethod
    def find(key, profiles):
        for p in profiles:
            if key == p.key:
                return p
        return None

    def update(self):
        profile = Profile.query.filter_by(uid=self.uid, grp=self.grp, key=self.key).first()
        if profile:
            profile.value = self.value
        else:
            db.session.add(self)

class Query(db.Model):

    __tablename__ = 'queries'

    id = db.Column(db.Integer, primary_key=True)
    uid = db.Column(db.Integer, db.ForeignKey('users.id'))
    tab = db.Column(db.String(100))
    mod = db.Column(db.String(100))
    name = db.Column(db.String(200))
    filters = db.Column(db.String)
    is_public = db.Column(db.Boolean)
    created_at = db.Column(db.DateTime, default=datetime.now)
    updated_at = db.Column(db.DateTime, default=datetime.now) 

